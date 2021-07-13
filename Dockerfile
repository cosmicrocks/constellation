FROM mozilla/sbt:latest as builder

ENV OPENJDK_TAG=8u202
ENV SBT_VERSION=1.5.4

WORKDIR /build
# Cache dependencies first
COPY project project
COPY keytool keytool
COPY schema schema
COPY src src
COPY ui ui
COPY wallet wallet
COPY build.sbt .
# Then build

RUN sbt "++ 2.12.10" "keytool/assembly" "++ 2.12.10" "wallet/assembly" && \
    mkdir -p ./src/main/resources/ui/ && \
    cd ui && sbt clean fullOptJS && cd .. &&\
    cp ./ui/target/scala-2.12/*js* ./src/main/resources/ui/ && \
    sbt "++ 2.12.10" assembly

FROM lwieske/java-8

COPY --from=builder /build/target/scala-2.12/constellation-assembly-2.24.10.jar /var/lib/constellation/cl-node.jar

RUN adduser -S -h /var/lib/constellation constellation

WORKDIR /var/lib/constellation/

USER constellation

EXPOSE 9000
EXPOSE 9001
EXPOSE 9002
EXPOSE 9003

COPY key.p12 key.p12
COPY whitelist whitelist