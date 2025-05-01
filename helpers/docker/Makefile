all: build

build:
	docker build -t icar/latest .

run: interactive
interactive:
	docker run -it --name icar icar/latest
interactive-saved:
	docker run -it --name icar -v ./path_to_icar_repo:/home/docker/icar icar/latest
host:
	docker run -p 8888:8888 --name icar -v ./path_to_icar_repo:/home/docker/icar icar/latest

clean-container:
	docker rm icar || true
clean-image:
	docker rmi icar/latest || true
clean: clean-container clean-image
