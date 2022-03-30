
# Docker Configuration

## Docker image

The pre-built PhytoFit Docker image is available on Docker hub here:
https://hub.docker.com/r/cioosatlantic/phytofit

## Building the Docker image

Building the Docker image should only need to be done rarely. Enabling newly developed app features will require pulling in the changes, rebuilding the Docker image, and pushing the new image to Docker hub. In Linux this looks like the following.

```
cd /path/to/Phytofit
git pull
docker build -t cioosatlantic/PhytoFit
docker push cioosatlantic/phytofit
```

## Deploying the application

Setting up the pre built Docker image on a new host using Docker compose can be done with the following steps.

```
cd /path/to/PhytoFit
git pull
docker-compose up -d
```

You should then be able to view the application locally at http://127.0.0.1:7520.

## Data downloads

The `PhytoFit/data` subdirectory is mounted to the Docker image. On first running docker-compose up, the contents of this folder will be brought in and then all new FST files will be downloaded. The directory is bind mounted in Docker, so new files will appear in the `PhytoFit/data` directory as well.
