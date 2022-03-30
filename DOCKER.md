
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
