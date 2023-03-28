FROM ocaml/opam:alpine-3.16-ocaml-4.14 as build
RUN sudo ln -f /usr/bin/opam-2.1 /usr/bin/opam
RUN cd ~/opam-repository && git pull origin -q master && git reset --hard e39502027c644408419cbba3f8bbbcfd71603476 && opam update
RUN opam repo add alpha https://github.com/kit-ty-kate/opam-alpha-repository.git
RUN sudo apk add gmp-dev libffi-dev linux-headers pkgconf
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