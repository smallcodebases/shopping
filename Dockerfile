# Build and minify JS and CSS
FROM --platform=$BUILDPLATFORM node:alpine AS elm-build
WORKDIR /stuff
RUN npm install -g elm@0.19.1 terser lightningcss-cli
COPY elm.json Main.elm index.js main.css ./
RUN elm make Main.elm --optimize --output elm.js \
  && terser elm.js --compress 'pure_funcs=[F2,F3,F4,F5,F6,F7,F8,F9,A2,A3,A4,A5,A6,A7,A8,A9],pure_getters,keep_fargs=false,unsafe_comps,unsafe,passes=2' --mangle --output main.js \
  && terser index.js --compress --mangle --output index.min.js \
  && lightningcss --minify main.css --output-file main.min.css

# Hash static assets and patch source files
FROM --platform=$BUILDPLATFORM alpine AS hash-and-patch
WORKDIR /stuff
COPY --from=elm-build /stuff/main.js ./main.js
COPY --from=elm-build /stuff/index.min.js ./index.js
COPY --from=elm-build /stuff/main.min.css ./main.css
COPY index.html main.go ./
RUN mkdir static \
  && INDEX_JS_HASH=$(sha256sum index.js | cut -c1-64) \
  && MAIN_CSS_HASH=$(sha256sum main.css | cut -c1-64) \
  && MAIN_JS_HASH=$(sha256sum main.js | cut -c1-64) \
  && mv index.js static/${INDEX_JS_HASH}.js \
  && mv main.css static/${MAIN_CSS_HASH}.css \
  && mv main.js static/${MAIN_JS_HASH}.js \
  && sed -i \
    -e "s|index\.js|${INDEX_JS_HASH}.js|" \
    -e "s|main\.css|${MAIN_CSS_HASH}.css|" \
    -e "s|main\.js|${MAIN_JS_HASH}.js|" \
    index.html \
  && mv index.html static/ \
  && sed -i \
    -e "s|serveStaticFile(\"GET /index\.js\", \"index\.js\")|serveHashedStaticFile(mux, \"GET /${INDEX_JS_HASH}.js\", \"${INDEX_JS_HASH}.js\")|" \
    -e "s|serveStaticFile(\"GET /main\.css\", \"main\.css\")|serveHashedStaticFile(mux, \"GET /${MAIN_CSS_HASH}.css\", \"${MAIN_CSS_HASH}.css\")|" \
    -e "s|serveStaticFile(\"GET /main\.js\", \"main\.js\")|serveHashedStaticFile(mux, \"GET /${MAIN_JS_HASH}.js\", \"${MAIN_JS_HASH}.js\")|" \
    main.go

# Build the Go binary
FROM --platform=$BUILDPLATFORM golang:1.26.0-alpine AS go-build
WORKDIR /stuff
ARG TARGETARCH TARGETOS
COPY go.mod go.sum ./
RUN go mod download
COPY --from=hash-and-patch /stuff/main.go main.go
COPY queries/ queries/
COPY migrations/ migrations/
RUN --mount=type=cache,target=/root/.cache/go-build CGO_ENABLED=0 GOOS=$TARGETOS GOARCH=$TARGETARCH go build -ldflags="-s -w" -trimpath -o shopping .
RUN mkdir -p /var/lib/shopping

# Final image: just the binary and static files
FROM scratch
WORKDIR /shopping
COPY --from=go-build /var/lib/shopping /var/lib/shopping
COPY --from=go-build /stuff/shopping /bin/shopping
COPY --from=hash-and-patch /stuff/static ./
COPY favicon.svg manifest.webmanifest ./
EXPOSE 80
ENTRYPOINT ["/bin/shopping", "serve"]
