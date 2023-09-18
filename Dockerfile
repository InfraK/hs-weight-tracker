FROM haskell:9.2.8-slim as builder

WORKDIR /project

RUN apt-get update && apt-get install -y postgresql libpq-dev

COPY hs-weight-tracker.cabal ./

RUN cabal update

COPY . .

RUN cabal build && \
    mkdir /project/build && \
    cp $(cabal list-bin app) /project/build/app

FROM debian:10-slim as runner

RUN apt-get update && apt-get install -y --no-install-recommends \
    ca-certificates dpkg-dev libc6-dev libffi-dev libgmp-dev libnuma-dev libtinfo-dev make netbase xz-utils zlib1g-dev \
    postgresql libpq-dev && \
    rm -rf /var/lib/apt/lists/*

WORKDIR /project

COPY --from=builder /project/build/app ./

CMD ["/project/app"]
