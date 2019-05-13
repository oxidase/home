# -*- sh -*- (Bash only)
#

[ -r /usr/local/lib/bazel/bin/bazel-complete.bash ] && source /usr/local/lib/bazel/bin/bazel-complete.bash

# Some users have aliases such as bt="bazel test" or bb="bazel build", this
# completion function allows them to have auto-completion for these aliases.
_bazel__complete_target_stdout() {
  local cur=$(_bazel__get_cword) word command commands displacement workspace

  # Determine command: "" (startup-options) or one of $BAZEL_COMMAND_LIST.
  case "$1" in
      bb)
          commands=build
          ;;
      bt)
          commands=test
          ;;
      br)
          commands=run
          ;;
      *)
          command="$1"
          ;;
  esac

  workspace="$(_bazel__get_workspace_path)"
  displacement="$(_bazel__get_displacement ${workspace})"

  XCOMPREPLY=""
  for command in $commands ; do
      _bazel__to_compreply "$(_bazel__expand_target_pattern "$workspace" "$displacement" \
            "$cur" "$(_bazel__expansion_for $command)")"
      if [[ ${XCOMPREPLY} != ${COMPREPLY} ]] ; then
          XCOMPREPLY="${XCOMPREPLY}${COMPREPLY}"
      fi
  done
  COMPREPLY=${XCOMPREPLY}
}

# default completion for bazel
complete -F _bazel__complete -o nospace bz
complete -F _bazel__complete_target_stdout -o nospace br
complete -F _bazel__complete_target_stdout -o nospace bb
complete -F _bazel__complete_target_stdout -o nospace bt
