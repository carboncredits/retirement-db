FROM ocaml/opam:alpine-3.15-ocaml-5.0@sha256:1bc32becbcee59efc974895fdf97bc7593dc5131f58a3244f938819e280d396f as build
RUN cd ~/opam-repository && git pull origin -q master && git reset --hard d079370f3d78e090f895b3270d17398d88ed83d4 && opam update
RUN opam repo add alpha https://github.com/kit-ty-kate/opam-alpha-repository.git
RUN sudo apk add gmp-dev libffi-dev linux-headers
COPY --chown=opam retirement.opam retirement-data.opam /src/
RUN opam pin -yn /src/
WORKDIR /src
RUN opam install -y --deps-only --with-test .
ADD --chown=opam . .
RUN opam exec -- dune build @runtest
RUN opam exec -- dune build --profile release ./_build/install/default/bin/retirement

FROM alpine:3.15
RUN apk update && apk add gmp-dev libffi-dev libexecinfo-dev linux-headers curl git
WORKDIR /var/lib/retirement
EXPOSE 9090
ENTRYPOINT ["/usr/local/bin/retirement"]
COPY --from=build /src/_build/install/default/bin/retirement /usr/local/bin/