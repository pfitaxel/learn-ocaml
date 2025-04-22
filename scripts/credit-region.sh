#!/usr/bin/env bash
# Author: Erik Martin-Dorel, 2023
# License: MIT

set -o pipefail

if [ $# -lt 3 ]; then
    cat <<EOF
Usage: ./credit-region.sh File LineBeg LineEnd [BeyondOauthMoodleDev]

  e.g. ./credit-region.sh ../src/app/learnocaml_upgrade_main.ml 1 "" "true"
EOF
    exit 1
fi

git_blame_wrapper() {
    local File="$1"
    local LineBeg="$2"
    local LineEnd="$3"
    git blame -L"$LineBeg,$LineEnd" -M -w "$File" --line-porcelain | grep -e "^author " -e "^author-mail " | perl -wpe 's/^author(?:-mail)? //; if($.%2){s/\n/ /;}' | sort | uniq -c
    # already follows the .mailmap file.
}

# https://github.com/ocaml-sf/learn-ocaml/pull/362/commits/2847c36d34f342d919272b9e0885a900166c6aec
# git log --pretty='tformat:%an <%ae>' 2847c36d34f342d919272b9e0885a900166c6aec^..oauth-moodle-dev | sort -u

# todo: setup in .mailmap: # git log --pretty='tformat:%an <%ae>' | sort -u

oauth_moodle_dev_authors() {
    local hardcodedMode=true
    if [ "$hardcodedMode" = 'true' ]; then
# todo: keep this list up-to-date, until oauth-moodle-dev feats live in master!
        cat <<EOF
erik@martin-dorel.org
45235251+Aleridia@users.noreply.github.com
6310153+agrn@users.noreply.github.com
leo.segond@master-developpement-logiciel.fr
louis.ayroles@master-developpement-logiciel.fr
nassim.mourabit@master-developpement-logiciel.fr
77079482+Plictox@users.noreply.github.com
EOF
    else
        git log --pretty='tformat:%aE' 2847c36d34f342d919272b9e0885a900166c6aec^..oauth-moodle-dev | sort -u
    fi
}

main() {
    local File="$1"
    local LineBeg="$2"
    local LineEnd="$3"
    local BeyondOauthMoodleDev="$4"
    # todo: type verifications
    if [ -z "$BeyondOauthMoodleDev" ]; then
        git_blame_wrapper "$File" "$LineBeg" "$LineEnd" | grep $( printf -- '-e %s ' $(oauth_moodle_dev_authors) ) | tee /dev/stderr | { sleep 0.05; echo; sed -e 's/^ *[0-9][0-9]* /Co-authored-by: /' ; }

    else
        git_blame_wrapper "$File" "$LineBeg" "$LineEnd" | tee /dev/stderr | { sleep 0.05; echo; sed -e 's/^ *[0-9][0-9]* /Co-authored-by: /' ; }
    fi
}

main "$1" "$2" "$3" "$4"
