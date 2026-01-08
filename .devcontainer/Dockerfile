FROM --platform=linux/amd64 mcr.microsoft.com/vscode/devcontainers/base:ubuntu

RUN apt-get -y update
RUN apt-get -y upgrade
RUN apt-get -y install gcc build-essential python3 python-is-python3 nasm clang lldb lld
RUN wget https://download.racket-lang.org/installers/9.0/racket-9.0-x86_64-linux-buster-cs.sh
RUN chmod +x ./racket-9.0-x86_64-linux-buster-cs.sh
RUN ./racket-9.0-x86_64-linux-buster-cs.sh