#!/bin/zsh
# Rscript stand-in that always launches R 4.5.2 (the only local R with dRiftDM).
# Used as the `rscript` argument of parallel::makeCluster() in the narrowA
# simulation scripts: the framework's bin/Rscript ignores R_HOME and starts the
# default R, which cannot load the 4.5 library. Translates the Rscript call
# that parallel builds (`--default-packages=... -e EXPR MASTER=... PORT=...`)
# into the equivalent call of the R 4.5 binary.
RH=/Library/Frameworks/R.framework/Versions/4.5-arm64/Resources
export R_HOME="$RH"
typeset -a pre post
while (( $# )); do
  case "$1" in
    --default-packages=*) export R_DEFAULT_PACKAGES="${1#--default-packages=}" ;;
    -e) pre+=("-e" "$2"); shift ;;
    --no-echo|--no-restore|--vanilla|--no-save|--no-init-file|--no-site-file|--no-environ) pre+=("$1") ;;
    *) post+=("$1") ;;
  esac
  shift
done
exec "$RH/bin/exec/R" --no-echo --no-restore "${pre[@]}" --args "${post[@]}"
