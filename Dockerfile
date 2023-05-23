########################################################################################################################
# co-pilot: R-Shiny-characterstic.
# This co-pilot implements the `R-Shiny`-characterstic.
# Concrete move-store-r-shiny apps could use this image ase its base.
########################################################################################################################

FROM rocker/geospatial:4.2.1

LABEL maintainer = "couchbits GmbH <us@couchbits.com>"

# Security Aspects
# group `staff` b/c of:
# When running rocker with a non-root user the docker user is still able to install packages.
# The user docker is member of the group staff and could write to /usr/local/lib/R/site-library.
# https://github.com/rocker-org/rocker/wiki/managing-users-in-docker
#  (to simplify things we use the same directory as for co-pilot-r)
RUN useradd --create-home --shell /bin/bash moveapps --groups staff
USER moveapps:staff

WORKDIR /home/moveapps/co-pilot-r

# renv
ENV RENV_VERSION 0.15.5
RUN R -e "install.packages('remotes', repos = c(CRAN = 'https://cloud.r-project.org'))"
RUN R -e "remotes::install_github('rstudio/renv@${RENV_VERSION}')"
COPY --chown=moveapps:staff renv.lock .Rprofile ./
COPY --chown=moveapps:staff renv/activate.R renv/settings.dcf ./renv/

# copy the SDK
COPY --chown=moveapps:staff src/ ./src/
COPY --chown=moveapps:staff data/ ./data/
COPY --chown=moveapps:staff www/ ./www/
COPY --chown=moveapps:staff co-pilot-sdk.R ShinyModule.R boot.R start-process.sh ./
# configure shiny
COPY --chown=moveapps:staff Rprofile.site /usr/local/lib/R/etc/
RUN mkdir ./data/output
# and restore the R libraries
RUN R -e 'renv::restore()'

# shiny port
EXPOSE 3838

ENTRYPOINT ["/bin/bash"]
