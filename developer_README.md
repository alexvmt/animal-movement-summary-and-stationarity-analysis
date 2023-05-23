# Developing with the MoveApps SDK

## General notes

- get an overview with the help of the [user manual](https://docs.moveapps.org/#/hello_world_app)
- your app codes goes to `./ShinyModule.R`
- setup your app arguments and your environment by adjusting `./co-pilot-sdk.R`
- to run your app code locally execute the file `./co-pilot-sdk.R`

## R Library management

The template is prepared to use [`renv` as a dependency manager](https://rstudio.github.io/renv/articles/renv.html) - but is disabled ("opt-in") by default. You can [activate `renv` with `renv::activate()`](https://rstudio.github.io/renv/articles/renv.html#uninstalling-renv) and then use it in the [usual `renv` workflow](https://rstudio.github.io/renv/articles/renv.html#workflow). This will setup your local development environment quickly and in an isolated manner.

## Docker support

- at the end your app will be executed on MoveApps in a Docker container.
- if you like you can test your app in the almost final environment by running your app locally in a docker container:

1. add each R library you added to your app via `renv` to the docker image by adding eg. `RUN R -e 'remotes::install_version("foreach")'` to the `./Dockerfile` before `RUN R -e 'renv::restore()'`
1. set a working title for your app by `export MY_MOVEAPPS_APP=hello-world`
1. build the Docker image locally by `docker build -t $MY_MOVEAPPS_APP .`
1. execute the image with `docker run --rm --name $MY_MOVEAPPS_APP -it -p 3838:3838 $MY_MOVEAPPS_APP`
1. you will get a `bash` terminal of the running container. There you can get a R console by `R` or simply start your app by invoking `/home/moveapps/co-pilot-r/start-process.sh` inside the running container.
