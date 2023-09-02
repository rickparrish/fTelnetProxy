FROM mono:latest
WORKDIR /app
COPY ./app /app/
ENTRYPOINT ["mono", "/app/bin/Release/fTelnetProxy.exe"]

