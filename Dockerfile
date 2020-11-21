FROM haskell:8.8
WORKDIR FLTask
COPY . .
CMD stack run
