FROM ubuntu:22.04 AS build

RUN apt-get update && \
    apt-get install -y \
      build-essential curl libffi-dev libffi8 libgmp-dev libgmp10 libncurses-dev libncurses6 libtinfo6 \
      zlib1g-dev \
      locales unzip wget git
RUN sed -i '/en_US.UTF-8/s/^# //g' /etc/locale.gen && \
    locale-gen
ENV LANG en_US.UTF-8
ENV LANGUAGE en_US:en
ENV LC_ALL en_US.UTF-8
RUN useradd -m cryptol-compiler
COPY --chown=cryptol-compiler:cryptol-compiler . /cryptol-compiler
USER cryptol-compiler
WORKDIR /cryptol-compiler
RUN mkdir -p rootfs/usr/local/bin

ARG CABALVER="3.10.1.0"
ARG GHCVER="9.2.8"
ENV PATH=/cryptol-compiler/rootfs/usr/local/bin:/home/cryptol-compiler/.local/bin:/home/cryptol-compiler/.ghcup/bin:$PATH
RUN mkdir -p /home/cryptol-compiler/.local/bin && \
    curl -L https://downloads.haskell.org/~ghcup/0.1.19.4/x86_64-linux-ghcup-0.1.19.4 -o /home/cryptol-compiler/.local/bin/ghcup && \
    chmod +x /home/cryptol-compiler/.local/bin/ghcup
RUN mkdir -p /home/cryptol-compiler/.ghcup && \
    ghcup --version && \
    ghcup install cabal ${CABALVER} && \
    ghcup install ghc ${GHCVER} && \
    ghcup set ghc ${GHCVER}
COPY cabal.GHC-${GHCVER}.config cabal.project.freeze
COPY cabal.project.dockerfile cabal.project.local
RUN cabal v2-update && \
    cabal v2-build -j exe:cryptol-compiler && \
    cp $(cabal list-bin exe:cryptol-compiler) rootfs/usr/local/bin
USER root
RUN chown -R root:root /cryptol-compiler/rootfs

FROM ghcr.io/galoisinc/cryptol-remote-api:3.0.0

USER root
RUN apt-get update && \
    apt-get install -y \
      build-essential curl vim && \
    apt-get clean && \
    rm -rf /var/lib/apt/lists/*
USER cryptol
COPY --from=ghcr.io/galoisinc/cryptol:3.0.0 /usr/local/bin/cryptol /usr/local/bin/cryptol
COPY --from=build /cryptol-compiler/rootfs/usr/local/bin/cryptol-compiler /usr/local/bin/cryptol-compiler

# Get Rust
RUN curl https://sh.rustup.rs -sSf | bash -s -- -y --profile minimal --default-toolchain 1.72.0
ENV PATH=/root/.cargo/bin:${PATH}

ENTRYPOINT ["/usr/bin/bash"]
