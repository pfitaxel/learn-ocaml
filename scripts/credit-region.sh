#!/usr/bin/env bash
# Author: Erik Martin-Dorel, 2023
# License: MIT

if [ $# -lt 3 ]; then
    cat <<EOF
Usage: ./credit-region.sh File LineBeg LineEnd

  e.g. ./credit-region.sh ../src/app/learnocaml_upgrade_main.ml 1 ""
EOF
    exit 1
fi

git_blame_wrapper() {
    local File="$1"
    local LineBeg="$2"
    local LineEnd="$3"
    git blame -L"$LineBeg,$LineEnd" -M "$File" --line-porcelain | grep -e "^author " -e "^author-mail " | perl -wpe 's/^author(?:-mail)? //; if($.%2){s/\n/ /;}' | sort | uniq -c
    # already follows the .mailmap file.
}

# https://github.com/ocaml-sf/learn-ocaml/pull/362/commits/2847c36d34f342d919272b9e0885a900166c6aec
# git log --pretty='tformat:%an <%ae>' 2847c36d34f342d919272b9e0885a900166c6aec^..oauth-moodle-dev | sort -u

# todo: setup in .mailmap: # git log --pretty='tformat:%an <%ae>' | sort -u

oauth_moodle_dev_authors() {
    local testMode=false
    if [ "$testMode" = 'true' ]; then
        echo alban.gruin@univ-tlse3.fr
    else
        git log --pretty='tformat:%aE' 2847c36d34f342d919272b9e0885a900166c6aec^..oauth-moodle-dev | sort -u
    fi
}

main() {
    local File="$1"
    local LineBeg="$2"
    local LineEnd="$3"
    # todo: type verifications
    git_blame_wrapper "$File" "$LineBeg" "$LineEnd" | tee /dev/stderr | { sleep 0.05s; echo; sed -e 's/^ *[0-9]\+ /Co-authored-by: /' | grep $( printf -- '-e %s ' $(oauth_moodle_dev_authors) ) ; }
 }

main "$1" "$2" "$3"
# "${4:-2847c36d34f342d919272b9e0885a900166c6aec^..}"
