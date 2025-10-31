FROM sbtscala/scala-sbt:eclipse-temurin-jammy-17.0.8.1_1_1.9.6_3.3.1 AS build

WORKDIR /app

COPY build.sbt ./
COPY project/ ./project/

RUN sbt update

COPY src/ ./src/

RUN sbt assembly

FROM eclipse-temurin:17-jre-jammy

WORKDIR /app

COPY --from=build /app/target/scala-3.3.0/magicconch-assembly-0.1.0-SNAPSHOT.jar ./magicconch.jar

CMD ["java", "-jar", "magicconch.jar"]