FROM haskell:8
RUN mkdir /app
ADD package.yaml /app
ADD stack.yaml /app
WORKDIR /app
# maybe build elm app here
RUN stack install --install-ghc
ENTRYPOINT ["stack" "exec" "flashcards"]
