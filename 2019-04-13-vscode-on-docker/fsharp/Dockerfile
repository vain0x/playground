FROM ubuntu:18.04

RUN apt-get update
RUN apt-get install -y curl gpg libasound2 wget

# Install Mono
RUN apt-get install -y gnupg ca-certificates \
    && apt-key adv --keyserver hkp://keyserver.ubuntu.com:80 --recv-keys 3FA7E0328081BFF6A14DA29AA6A19B38D3D831EF \
    && echo "deb https://download.mono-project.com/repo/ubuntu stable-bionic main" | tee /etc/apt/sources.list.d/mono-official-stable.list \
    && apt-get update \
    && apt-get install -y mono-devel

# Install .NET Core
RUN wget -q https://packages.microsoft.com/config/ubuntu/18.04/packages-microsoft-prod.deb \
    && dpkg -i packages-microsoft-prod.deb

# Install F#
RUN apt-get update \
    && apt-get install -y fsharp

# Install VSCode
# https://code.visualstudio.com/docs/setup/linux
RUN curl https://packages.microsoft.com/keys/microsoft.asc | gpg --dearmor > microsoft.gpg \
    && install -o root -g root -m 644 microsoft.gpg /etc/apt/trusted.gpg.d/ \
    && sh -c 'echo "deb [arch=amd64] https://packages.microsoft.com/repos/vscode stable main" > /etc/apt/sources.list.d/vscode.list' \
    && apt-get install apt-transport-https \
    && apt-get update \
    && apt-get install -y code # or code-insiders

RUN apt-get install -y libx11-xcb-dev

# Install .NET Core (fix)
RUN apt-get install -y software-properties-common \
    && add-apt-repository universe \
    && apt-get install -y apt-transport-https \
    && apt-get update \
    && apt-get install -y dotnet-sdk-2.2

ENV DOTNET_CLI_TELEMETRY_OPTOUT=1

RUN apt-get install -y git sudo

# Add non-root user.
RUN groupadd -r worker \
    && useradd -r -g worker worker \
    && mkdir -p /home/worker \
    && chown -R worker.worker /home/worker

USER worker
WORKDIR /home/worker

# Install ionide-fsharp to VSCode.
RUN code --install-extension ms-vscode.csharp \
    && code --install-extension ionide.ionide-fsharp

ADD --chown=worker:worker cli/.vscode /home/worker/cli.vscode

# Create initial project.
ADD init.d init.d

ENTRYPOINT ["/bin/bash"]
    CMD ["init.d"]
