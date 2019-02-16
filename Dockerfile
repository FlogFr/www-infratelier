# Builder container to compile the haskell binary
FROM debian:stretch-slim AS builder
MAINTAINER "grignon.florian@gmail.com"

# Install dependencies requirements
RUN apt update
RUN apt install -y curl xz-utils gcc make libpq-dev libgmp-dev zlib1g-dev

# Install ghcup to the debian stretch-slim
RUN curl https://gitlab.haskell.org/haskell/ghcup/raw/master/bootstrap-haskell -sSf | BOOTSTRAP_HASKELL_NONINTERACTIVE=y sh
# RUN curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | BOOTSTRAP_HASKELL_NONINTERACTIVE=y sh

COPY . /code
WORKDIR /code
RUN PATH=/root/.ghcup/bin:${PATH} make buildbackend
RUN find . |grep 'infratelier'

# Now we copy the previously build binary into a clean container
FROM debian:stretch-slim
MAINTAINER "grignon.florian@gmail.com"

EXPOSE 9000/tcp
EXPOSE 9001/tcp

# Install the requirements
RUN apt update
RUN apt install -y libpq5 libnuma1 libpq-dev

# Create a group and user
RUN useradd --create-home -d /home/infratelier -s /bin/bash infratelier

COPY --from=builder /code/dist-newstyle/build/x86_64-linux/ghc-8.8.3/infratelier-0.1.0.0/x/infratelier/noopt/build/infratelier/infratelier /infratelier
COPY ./src/templates /home/infratelier/templates

USER root
WORKDIR /home/infratelier
CMD /infratelier /var/infratelier/config.yml /var/infratelier/database.json
