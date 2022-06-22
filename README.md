# Haskell Palette - a full stack Haskell application

## Application DEMO

Application is available at [https://haskell-palette.herokuapp.com/](https://haskell-palette.herokuapp.com/).
After every successful commit to the `main` branch the newest version of application is automatically updated
on the server with the help of prepared CI/CD scripts and configurations.

## Project goals

The project goals are:
1. Design and play with Haskell by implementing a frontend and backend for a web application which allows 
   users to generate color palettes from uploaded photos.
2. Implement different algorithms for palette generation in Haskell.
3. Try working with Haskell libraries for processing the data in parallel.
4. Prepare a ready-to-go environment for a project in Haskell that would be easy to scale including:
   - convenient dev environment for designing the application locally
   - configured CI/CD for Haskell app deployment

## Project structure

### API

We decided to prepare an open api that is capable of generating color palettes with different 
methods selected by user. The basic usage of API includes:

#### POST /upload

The API request includes the selection of algorithm, requested number of colors and the image data 
encoded using Base64 algorithm for image encoding.
The possible inputs for algorithms are:
- `"histogram"`
- `"median_cut"`
- `"k_means"`
- `"k_means_pp"`

##### Sample request
```
{
    "algorithm": "histogram",
    "image": "iVBORw0KGgoAAAANSUhEUgAAABMAAAASAgMAAADEYg36AAAADFBMVEVAAP8A/wD//wD/AABSvgk5AAAAIUlEQVR4nGNgYAgNDWEgg0wNYfj//+qqFUDyFQ7yFyoJADGDK+gsHi98AAAAAElFTkSuQmCC",
    "count": 4
}
```

#### Response 200

The response with `200` code contains the list of colors detected by specified algorithm on the image.
The colors are represented as hex color codes and are returned as a list of all colors returned by algorithm. 

##### Sample response
```
{
    colors: ["#00ff00", "#4000ff", "#ff0000", "#ffff00"]
}
```

### Haskell webserver

Webserver application is written with the usage of Scotty framework that allows to easily design 
RESTful application in a declarative manner. All server sources are included in `app` and `src` directories.

The most interesting parts of the server used in the project are: 
- Automatic paring of JSON requests and generating JSON responses with the help of the `aeson` library
  that uses `ToJSON` and `FromJSON` generators Haskell classes representing JSON objects.
- Working with images with Haskell libraries including `JuicyPixels` and `repa`.
- Implementation of the algorithms for palette generation.

### Elm (aka. almost Haskell) frontend

The frontend application uses Elm language which is a functional, strongly typed frontend language
that by its structure and syntax is really similar to Haskell. It brings a lot of type safety
and purity that Haskell developers are used to while it still compiles to Javascript.

## Prepared environments

### Dev environment

Dev environment is prepared to allow an easy start of the development of the application with no Haskell
compiler needed on the actual machine. The whole configuration (included in `.devcontainer` directory)
contains the configuration of Docker image that is capable of running in VS Code IDE as an active 
environment.

1. Make sure you have docker installed
2. Install [VS Code remote containers extension](https://marketplace.visualstudio.com/items?itemName=ms-vscode-remote.remote-containers)
3. Open repository in VS Code
4. Open command palette (ctrl+shift+p) and use "Remote Containers: Reopen in container" action
5. Use prepared `Makefile` commands to work interactively with `cabal` (`make run-dev` starts the local
   instance of the application, but first the application frontend should be compiled separately)

### Building frontend

Run `make build-frontend`. It will create an `index.html` in the `frontend` directory.
By default, the frontend is trying to reach the API on `/` so the path is relative to the frontend source.

### Running local docker image

Repository includes separate minimalistic configuration of the environment for running the application.
We've prepared our Docker images that are designed to compile frontend and backend of the application
and serve the compiled files in a separate environment.

Using single command `make run-image` builds local app image and starts the prepared application on `3000`
port of `localhost`. The whole process includes:
- preparing the environment for frontend compilation and the compilation itself
- preparing the environment for backend compilation and the compilation itself
- preparing pure image with compiled sources ready-to-run with `docker run` command (that is executed as last step
  of `make run-image`).

Our configuration of CI/CD also publishes automatically the newest image with app image on 
[dockerhub,](https://hub.docker.com/r/zpf2022/haskell-palette) so it can be just downloaded and 
launched with no local compilation process needed.
