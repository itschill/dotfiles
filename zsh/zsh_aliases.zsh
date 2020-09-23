# Zsh Aliases

#
# Files
#
alias -g vim="nvim"
alias -g nv="nvim"

alias -g grep="grep --color=auto"

alias -g lessn="less -FXMN"

alias -g tgz="tar cvzf"
alias -g tbz2="tar cvjf"
alias -g untar="tar xvf"
alias -g untgz="tar xvzf"
alias -g untbz2="tar xvjf"


#
# List
#
alias -g l="/opt/coreutils/bin/ls -h --color=auto"
alias -g ls="exa --icons --color=always --group-directories-first"
alias -g la="ls --all"
alias -g ll="la --long --color-scale"
alias -g lt="ll --tree"
alias -g l1="lt --level=2"
alias -g lr="ll -R"


#
# Directories
#
alias -g mkdir="mkdir -p"

alias .='cd `pwd`'
alias ..="cd .."
alias ...="cd ../.."
alias ....="cd ../../.."
alias .....="cd ../../../.."
alias ......="cd ../../../../.."
alias .......="cd ../../../../../.."
alias ........="cd ../../../../../../.."
alias .........="cd ../../../../../../../.."
alias ..........="cd ../../../../../../../../.."
alias ...........="cd ../../../../../../../../../.."
alias ............="cd ../../../../../../../../../../.."


#
# System
#
alias -g free="free -h"
alias dropcaches="sync && sudo sh -c 'echo 3 > /proc/sys/vm/drop_caches'"
alias mounted="mount | column -t"


#
# Cool Stuff
#
alias -g shh="&> /dev/null &"

alias -g myip="echo $(curl -sk https://ifconfig.me)"
alias weather="curl http://wttr.in"
alias moon="curl http://wttr.in/Moon"
alias music="cat /dev/urandom | hexdump -v -e '/1 \"%u\n\"' | awk '{ split(\"0,2,4,5,7,9,11,12\", a, \",\"); for (i = 0; i < 1; i += 0.0001) printf(\"%08X\n\", 100*sin(1382*exp((a[$1 % 8]/12)*log(2))*i)) }' | xxd -r -p | aplay -c 2 -f S32_LE -r 16000"
alias parrot="curl parrot.live"

alias forkbomb=":(){ :&:&};:"

alias -g genusr="gpw 1"
alias -g genpwd="pwgen -1sy"


#
# Text to Speech
#
say () {
    local text="$@"

    festival -b '(voice_cmu_us_slt_arctic_hts)' "(SayText \"$text\")"
}

remind () {
    local reminder="$1"
    local sleep_time="$2"

    (sleep $sleep_time && say "$reminder") &
}


#
# Network
#
alias renew-ip="sudo dhclient -r && sudo dhclient"
alias -g httpserver="python3 -m http.server"
alias checkutf8="echo -e \"\xE2\x98\xA0\""
alias checknet="ping 8.8.8.8"
alias checkdns="nslookup www.google.com"


#
# System Monitoring
#
alias -g temperature="paste <(cat /sys/class/thermal/thermal_zone*/type) <(cat /sys/class/thermal/thermal_zone*/temp) | column -s $'\t' -t | sed 's/\(.\)..$/.\1°C/'"


#
# Other
#
alias alert='notify-send --urgency=low -i "$([ $? = 0 ] && echo terminal || echo error)" "$(history|tail -n1|sed -e '\''s/^\s*[0-9]\+\s*//;s/[;&|]\s*alert$//'\'')"'
alias debug="set -o nounset; set -o xtrace"

alias glr="git pull --rebase"
alias gph="git push origin HEAD"
alias gphf="git push origin HEAD --force-with-lease"
alias gri="git rebase -i"
alias grim="git rebase -i master"
alias gcp="git cherry-pick"

alias whiteout="~/Downloads/magicwand 1,1 -t 20 -f image -r outside -m overlay -o 0"


#
# Config
#
alias aedit="$EDITOR $ZSH_ALIASES && source $ZSH_ALIASES"


#
# Functions
#
man() {
    LESS_TERMCAP_md=$'\e[01;31m' \
    LESS_TERMCAP_me=$'\e[0m' \
    LESS_TERMCAP_se=$'\e[0m' \
    LESS_TERMCAP_so=$'\e[01;44;33m' \
    LESS_TERMCAP_ue=$'\e[0m' \
    LESS_TERMCAP_us=$'\e[01;32m' \
    command man "$@"
}

cdls () {
    cd "$1" && ls
}

mkcd () {
    mkdir -p "$1" && cd "$1"
}

xml-tagvalue () {
    grep -oPm1 "(?<=<$2>)[^<]+" "$1"
}

git-unstage-pattern () {
    git diff | grepdiff "$1" --output-matching=hunk | git apply -R
}

git-rm-branches () {
    git branch | grep "$1" | xargs git branch -D
}

git-rm-remote-branches () {
    git branch -a | grep "$1" | cut -d"/" -f3 | xargs git push origin --delete
}

mv-files-ext () {
    local old="$1"
    local new="$2"

    for f in `ls *."$old"`; do
        local name=${f%%."$old"};
        mv "$name"."$old" "$name"."$new";
    done;
}

pdfcat () {
    if [ $# -ne 3 ]; then
        echo 'usage: pdfcat PDF_1 PDF_2 FINAL_PDF'
    else
        gs -q -sPAPERSIZE=letter -dNOPAUSE -dBATCH -sDEVICE=pdfwrite -sOutputFile="$3" "$1" "$2"
    fi
}

str-join () {
    local separator="$1"
    shift;
    echo $@ | sed -e "s/[[:space:]]\+/$separator/g"
}

find-jars () {
	local classname="$1"
    local location="${2:-.}"

    find "$location" -name '*.jar' -exec grep -Hls "$classname" {} \;
}

jwt-decode () {
    local jwt="$1"
    local parts=(`echo ${jwt//./ }`)

    echo "Header: Algorithm & Token Type"
    echo "${parts[1]}" | base64 -d | jq '.'
    echo "Payload: Data"
    echo "${parts[2]}" | base64 -d | jq '.'
    echo "Verify Signature: "
    echo "${parts[3]}" | base64
    echo -n "Expires on: "
    echo "${parts[2]}" | base64 -d | jq '.exp?' | xargs -I {} date -d @{} 2> /dev/null || echo "Never"
}


#
# Macros
#
mouse-record () {
    local delay="${1:-0}"
    local events="${2:-10000}"

    cnee --record -o events.xnr --mouse --events-to-record $events --time $delay
}

mouse-replay () {
    local delay="${1:-0}"

    cnee --replay -f events.xnr --time $delay
}
