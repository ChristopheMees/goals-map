FROM eclipse-temurin:25.0.1_8-jre-alpine-3.22
RUN apk add bash curl
COPY linux-install.sh linux-install.sh
RUN ./linux-install.sh
COPY app app
WORKDIR app
RUN clojure -M --main cljs.main --compile goals.app
COPY server server
RUN mkdir server/public/out
RUN mv out server/public
EXPOSE 8080
WORKDIR server
ENTRYPOINT ["clojure", "-M", "--main", "server.core"]