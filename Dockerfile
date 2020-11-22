FROM haskell:8.8
WORKDIR FLTask
COPY . .
RUN stack build
CMD stack run
