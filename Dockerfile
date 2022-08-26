FROM ocaml/opam:alpine-3.15-ocaml-5.0@sha256:1bc32becbcee59efc974895fdf97bc7593dc5131f58a3244f938819e280d396f as build
RUN cd ~/opam-repository && git pull origin -q master && git reset --hard c47b5947512e34e7e64e0cb5dad23a7613580e4b && opam update
RUN opam repo add alpha https://github.com/kit-ty-kate/opam-alpha-repository.git
RUN apk add gmp-dev libffi-dev linux-headers
COPY --chown=opam 4c-retirement.opam /src/
RUN opam pin -yn /src/
WORKDIR /src
RUN opam install -y --deps-only .
ADD --chown=opam . .
RUN opam exec -- dune build --profile release ./_build/install/default/bin/4c-retirement

FROM alpine:3.15
RUN apk update && apk add curl git
WORKDIR /var/lib/4c-retirement
ENTRYPOINT ["/usr/local/bin/4c-retirement"]
COPY --from=build /src/_build/install/default/bin/4c-retirement /usr/local/bin/