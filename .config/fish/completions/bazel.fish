# https://medium.com/@fabioantunes/a-guide-for-fish-shell-completions-485ac04ac63c
set -g bazel_commands analyze-profile aquery build canonicalize-flags clean coverage cquery dump fetch help info license \
             mobile-install mod print_action query run shutdown sync test vendor version

for bin in bazel bazelisk ;
    complete -f -c $bin -n "not __fish_seen_subcommand_from $bazel_commands" -a analyze-profile -d 'Analyzes build profile data.'
    complete -f -c $bin -n "not __fish_seen_subcommand_from $bazel_commands" -a aquery -d 'Analyzes the given targets and queries the action graph.'
    complete -f -c $bin -n "not __fish_seen_subcommand_from $bazel_commands" -a build -d 'Builds the specified targets.'
    complete -f -c $bin -n "not __fish_seen_subcommand_from $bazel_commands" -a canonicalize-flags -d 'Canonicalizes a list of bazel options.'
    complete -f -c $bin -n "not __fish_seen_subcommand_from $bazel_commands" -a clean -d 'Removes output files and optionally stops the server.'
    complete -f -c $bin -n "not __fish_seen_subcommand_from $bazel_commands" -a coverage -d 'Generates code coverage report for specified test targets.'
    complete -f -c $bin -n "not __fish_seen_subcommand_from $bazel_commands" -a cquery -d 'Loads, analyzes, and queries the specified targets w/ configurations.'
    complete -f -c $bin -n "not __fish_seen_subcommand_from $bazel_commands" -a dump -d 'Dumps the internal state of the bazel server process.'
    complete -f -c $bin -n "not __fish_seen_subcommand_from $bazel_commands" -a fetch -d 'Fetches external repositories that are prerequisites to the targets.'
    complete -f -c $bin -n "not __fish_seen_subcommand_from $bazel_commands" -a help -d 'Prints help for commands, or the index.'
    complete -f -c $bin -n "not __fish_seen_subcommand_from $bazel_commands" -a info -d 'Displays runtime info about the bazel server.'
    complete -f -c $bin -n "not __fish_seen_subcommand_from $bazel_commands" -a license -d 'Prints the license of this software.'
    complete -f -c $bin -n "not __fish_seen_subcommand_from $bazel_commands" -a mobile-install -d 'Installs targets to mobile devices.'
    complete -f -c $bin -n "not __fish_seen_subcommand_from $bazel_commands" -a print_action -d 'Prints the command line args for compiling a file.'
    complete -f -c $bin -n "not __fish_seen_subcommand_from $bazel_commands" -a query -d 'Executes a dependency graph query.'
    complete -f -c $bin -n "not __fish_seen_subcommand_from $bazel_commands" -a run -d 'Runs the specified target.'
    complete -f -c $bin -n "not __fish_seen_subcommand_from $bazel_commands" -a shutdown -d 'Stops the bazel server.'
    complete -f -c $bin -n "not __fish_seen_subcommand_from $bazel_commands" -a sync -d 'Syncs all repositories specified in the workspace file'
    complete -f -c $bin -n "not __fish_seen_subcommand_from $bazel_commands" -a test -d 'Builds and runs the specified test targets.'
    complete -f -c $bin -n "not __fish_seen_subcommand_from $bazel_commands" -a vendor -d 'Fetches external repositories into a folder specified by the flag --vendor_dir.'
    complete -f -c $bin -n "not __fish_seen_subcommand_from $bazel_commands" -a version -d 'Prints version information for bazel.'
end

function __fish_bazel_target
    set -l bazel (commandline -cpo)[1]
    set -l target (commandline -ct)
    switch $target
        case '//*:*'
            set -l comp (string match -r '//(.*):+(.*)' $target)[2]
            switch $argv
                case build
                    printf "%s\tTarget\n" ($bazel query $comp/... 2> /dev/null)
                case query
                    printf "%s\tTarget\n" ($bazel query $comp/... 2> /dev/null)
                case run
                    printf "%s\tBinary\n" ($bazel query "kind('.*(binary|application)',$comp:*)" 2> /dev/null)
                    printf "%s\tWrapper\n" ($bazel query "attr(binary,'.*',$comp:*)" 2> /dev/null)
                    printf "%s\tTest\n" ($bazel query "kind(test,$comp:*)" 2> /dev/null)
                case test
                    printf "%s\tTest\n" ($bazel query "kind(test,$comp:*)" 2> /dev/null)
            end
        case '//*'
            set -l comp (string sub $target -s 3)

            echo $comp
            set -l dirs (complete -C"nonexistentcommand $comp" | string match -vr '^bazel-' | string match -r '.*/$' | string replace -r '/$' '')
            for comp in $dirs
                if test -e "$comp/BUILD" -o -e "$comp/BUILD.bazel"
                    printf "//%s:\tTargets\n" $comp
                end
                if test (count (find $comp -maxdepth 1 -type d)) -gt 1
                    printf "//%s/\tDirectory %s\n" $comp
                end
            end
        case '' '/'
            printf '//\n'
    end
end

function __fish_is_option
    string match -r '^--.*' -- (commandline -ct) && return 0 || return 1
end

function __fish_bazel_options
    set -l bazel (commandline -cpo)[1]
    set -l commands (commandline -poc)
    set -e commands[1]
    for command in $commands
        if contains $command $bazel_commands
            set -l options ($bazel help $command 2>/dev/null | string match -r "^ +--.*")
            if test $status -eq 0
                for option in $options
                    set -l parts (string match -r "^ +--(\[no\])?([^ ]+) .*" $option)
                    if string length $parts[3]
                        printf -- "--no%s\tOption\n" $parts[3]
                        printf -- "--%s\tOption\n" $parts[3]
                    else
                        printf -- "--%s\tOption\n" $parts[2]
                    end
                end
           end
        end
    end
end

for bin in bazel bazelisk ;
    complete -f -c $bin -n "__fish_seen_subcommand_from help" -a (printf "%s\n" "$bazel_commands startup_options target-syntax info-keys")
    complete -f -c $bin -n "__fish_is_option" -a "(__fish_bazel_options)"
    for command in build run test query aquery cquery ;
        complete -f -c $bin -n "__fish_seen_subcommand_from $command; and not __fish_seen_subcommand_from (__fish_bazel_target $command); and not contains -- -- (commandline -opc)" \
                            -a "(__fish_bazel_target $command)"
    end
end

complete -c bz -w "bazel"
complete -c bb -w "bazel build"
complete -c br -w "bazel run"
complete -c bt -w "bazel test"
complete -c bazel-dev -w "bazel"
