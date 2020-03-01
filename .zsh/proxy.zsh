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
