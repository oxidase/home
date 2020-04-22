# Defined in /usr/local/share/fish/functions/fish_prompt.fish @ line 4
function _is_git_dirty
    echo (git status -s --ignore-submodules=dirty 2>/dev/null)
end

function fish_prompt --description 'Write out the prompt'
    set -l last_pipestatus $pipestatus
    set -l last_status $status
    set -l normal (set_color normal)

    # Color the prompt differently when we're root
    set -l color_cwd $fish_color_cwd
    set -l suffix '>'
    if contains -- $USER root toor
        if set -q fish_color_cwd_root
            set color_cwd $fish_color_cwd_root
        end
        set suffix '#'
    end

    # If we're running via SSH, change the host color.
    set -l color_host $fish_color_host
    if set -q SSH_TTY
        set color_host $fish_color_host_remote
    end

    # Write pipestatus
    set -l prompt_status (__fish_print_pipestatus $last_status " [" "]" "|" (set_color $fish_color_status) (set_color --bold $fish_color_status) $last_pipestatus)

    echo -n -s (set_color $fish_color_user) "$USER" $normal @ (set_color $color_host) (prompt_hostname) $normal ' ' (set_color $color_cwd) (prompt_pwd) $normal (fish_vcs_prompt) $normal $prompt_status $suffix " "
end

# set -g __fish_prompt_grey A3A3A3
# set -g __fish_git_prompt_char_cleanstate "✔"
# set -g __fish_git_prompt_char_conflictedstate "✖"
# set -g __fish_git_prompt_char_dirtystate "✚"
# set -g __fish_git_prompt_char_stagedstate "●"
# set -g __fish_git_prompt_char_stateseparator \U0020 #\Ue725
# set -g __fish_git_prompt_char_untrackedfiles "…"
# set -g __fish_git_prompt_char_upstream_ahead "↑"
# set -g __fish_git_prompt_char_upstream_behind "↓"
# set -g __fish_git_prompt_char_upstream_prefix ""
# set -g __fish_git_prompt_color $__fish_prompt_grey
# set -g __fish_git_prompt_color_branch $__fish_prompt_grey
# set -g __fish_git_prompt_color_cleanstate green
# set -g __fish_git_prompt_color_dirtystate blue
# set -g __fish_git_prompt_color_invalidstate red
# set -g __fish_git_prompt_color_stagedstate yellow
# set -g __fish_git_prompt_color_untrackedfiles $__fish_prompt_grey #$fish_color_normal
# set -g __fish_git_prompt_hide_untrackedfiles 1
# set -g __fish_git_prompt_show_informative_status 1
# set -g __fish_git_prompt_showcolorhints 1
# set -g __fish_git_prompt_showupstream "informative"

# # Needed if bobthefish is isntalled
# set -g theme_display_date no


# function fish_prompt --description 'Write out the prompt'

#     set -l last_status $status
#     set -l pathcolor $__fish_prompt_grey

#     if not set -q __fish_prompt_normal
#         set -g __fish_prompt_normal (set_color normal)
#     end

#     set_color $pathcolor
#     echo
#     printf '%s' (whoami)
#     printf '@'
#     printf '%s' (hostname|cut -d . -f 1)
#     printf ':'

#     # If current dir is not writable display it in red
#     if not [ -w (pwd) ]
#         set_color red
#     end
#     printf '%s' (pwd)

#     set_color normal
#     printf '%s' (__fish_git_prompt)

#     if not test $last_status -eq 0
#         set_color $fish_color_error
#     end
#     set_color $pathcolor

#     # Display Java version if in a java project
#     echo
#     if test -f pom.xml \
#             -o -f build.sbt \
#             -o -f build.gradle \
#             -o -f build.sc \
#             -o (count *.java) -gt 0
#         set_color blue
#         printf '\Ue256 %s' (java -version 2>&1 | grep -Eo '"(.*?)"' | head -1 | cut -d '"' -f2)
#     else if test (count *.py) -gt 0
#         set_color green
#         printf '\Ue235 %s' (python --version 2>&1 | grep -o "\d*\.\d*\.\d*")
#     end

#     printf '\UE0B1 '
#     set_color normal

# end
