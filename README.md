# SQUARE

This is interactive website for learning [SEQUEL](https://s3.us.cloud-object-storage.appdomain.cloud/res-files/2705-sequel-1974.pdf) predescessor [SQUARE](https://disciplinas.uvv.br/assets/disciplinas/bd1/square_1973.pdf).

## Examples

## Requirements

* [dotnet SDK](https://www.microsoft.com/net/download/core) v9.0 or higher
* [node.js](https://nodejs.org) v18+ LTS


## Development

Before doing anything, start with installing npm dependencies using `npm ci`.

Then to start development mode with hot module reloading, run:
```shell
npm start
```
This will start the development server after compiling the project, once it is finished, navigate to http://localhost:8080 to view the application .

To build the application and make ready for production:
```shell
npm run build
```
This command builds the application and puts the generated files into the `deploy` directory (can be overwritten in webpack.config.js).

### Tests

The template includes a test project that ready to go which you can either run in the browser in watch mode or run in the console using node.js and mocha. To run the tests in watch mode:
```
npm run test:live
```
This command starts a development server for the test application and makes it available at http://localhost:8085.

To run the tests using the command line and of course in your CI server, you have to use the mocha test runner which doesn't use the browser but instead runs the code using node.js:
```
npm test
```