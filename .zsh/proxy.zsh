PROXY=http://proxy.ksc.kwansei.ac.jp:80803

kgproxy(){
    export http_proxy=http://proxy.ksc.kwansei.ac.jp:8080
    export https_proxy=http://proxy.ksc.kwansei.ac.jp:8080
    export ftp_proxy=http://proxy.ksc.kwansei.ac.jp:8080
}

unproxy(){
    unset http_proxy
    unset https_proxy
    unset ftp_proxy
}

PROXY=http://proxy.ksc.kwansei.ac.jp:8080

if /sbin/iwconfig wlp2s0 | grep -o "lsnl" >/dev/null ; then
    export http_proxy=$PROXY
    export https_proxy=$PROXY
    export ftp_proxy=$PROXY
else
    unset http_proxy
    unset https_proxy
    unset ftp_proxy
fi
