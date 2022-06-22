# Haskell Palette - full stack Haskell application

## Application DEMO

Application is available at [https://haskell-palette.herokuapp.com/](https://haskell-palette.herokuapp.com/).
After every successful commit to `main` branch the newest version of application is automatically updated
on server with the help of prepared CI/CD scripts and configurations.

## Project goals

The project goals are:
1. design and play with Haskell project containing the implementation 
   of frontend and backend for web application which allows users to generate 
   colors palettes from uploaded photos
2. implement different algorithms for palette generation in Haskell
3. Try working with Haskell libraries for processing the data in parallel
4. prepare ready-to-go environment for project in Haskell that would be easy to scale including:
   - convenient dev environment for designing the application locally
   - configured CI/CD for Haskell app deployment

## Project structure

### API

We decided to prepare open api that is capable of generating color palettes with different 
methods selected by user. The basic usage of API includes

#### POST /upload

The API request includes the selection of algorithm and the image data encoded using Base64 algorithm
for image encoding.
The possible inputs for algorithms are:
- `"histogram"`
- `"median_cut"`
- `"k_means"`
- `"k_means_pp"`

##### Sample request
```
{
    "algorithm": "histogram",
    "image": "iVBORw0KGgoAAAANSUhEUgAAABMAAAASAgMAAADEYg36AAAADFBMVEVAAP8A/wD//wD/AABSvgk5AAAAIUlEQVR4nGNgYAgNDWEgg0wNYfj//+qqFUDyFQ7yFyoJADGDK+gsHi98AAAAAElFTkSuQmCC"
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
RESTful application in declarative manner. All server sources are included in `app` and `src` directories.

The most interesting parts of the server used in the project are: 
- automatic paring of json request and generating json responses with the help of the `aeson` library
  that uses `ToJSON` and `FromJSON` generators Haskell classes representing json objects
- working with images with Haskell libraries including `JuicyPixels` and `repa`
- implementation of algorithms for palette generation

### Elm (aka. almost Haskell) frontend

Frontend application uses Elm language which was designed sa functional frontend language
that by its structure and grammar is really similar to Haskell. It brings a lot of type safety
and purity that Haskell developers are used to while it still compiles to Javascript.

## Prepared environments

### Dev environment

Dev environment is prepared to allow easily start the development of application with no haskell
compiler needed on actual machine. The whole configuration (included in `.devcontainer` directory)
contains the configuration of Docker image that is capable of running in VS Code IDE as active 
environment replacing dev local environment.

1. Make sure you have docker installed
2. Install [VS Code remote containers extension](https://marketplace.visualstudio.com/items?itemName=ms-vscode-remote.remote-containers)
3. Open repository in VS Code
4. Open command palette (ctrl+shift+p) and use "Remote Containers: Reopen in container" action
5. Use prepared `Makefile` commands to work interactively with `cabal` (`make run-image` starts the local
   instance of the application, but first the application frontend should be compiled separately)

### Building frontend

Run `make build-frontend`. It will create an `index.html` in the `frontend` directory.
By default, the frontend is trying to reach the API on `/` so the path is relative to the frontend source.
