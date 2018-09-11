FROM ubuntu:18.04
RUN apt-get update && apt-get install -y libpq-dev
ADD exe /bin
ADD static /static
CMD ["exe"]
