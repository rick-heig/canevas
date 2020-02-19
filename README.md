# Canevas

Work in progress...  

This is the source code repository for my Master thesis project on CNV assessment on whole-genome sequencing data. It will serve as a basis for the development of more mature tools.

See also : https://github.com/rick-heig/vcfotographer


## Usage

### Extract signals from BAM file

### Generate VCFs from BAM file and signals

## Installation

In order to use the Canevas software there are two options:

- Create a docker image and run the application in a container. This is the most straightforward method but only gives access to the tools available from running the `canevas` executable.
- Use the development build, this means building with the Scala build tool `sbt` from source. This allows to use every function and sub-tool of the framework in an interactive Scala console. (also allows to run the `canevas` software as above).

Both methods start by cloning this repository :

```
git clone https://github.com/rick-heig/canevas.git
```

### Docker
A `Dockerfile` is provided in order to generate the docker image.

```
cd canevas # move into the repository
docker build -t rwk/canevas .
```

This will take a few minutes in order to download all the required dependencies and build the software.

Once the docker image is created it can be run in a container with the following command :

```
docker run rwk/canevas canevas [args]
```

In order to work with actual files, load the directories you need as [volumes](https://docs.docker.com/storage/volumes/) as shown below :

```
docker run --user $(id -u):$/$(id -g) -v /some/input/directory:/usr/input:ro -v /some/output/directory:/usr/output rwk/canevas canevas signal -b /usr/input/myBamFile.bam -o /usr/output
```

The `--user $(id -u):$/$(id -g)` option will with the same user and group ids in the docker machine than the user on the host. This is helpful so that the generated files are not owned by root:root but by the correct user.

The `-v <host_directory>:<container_directory>` options mount the host directories inside the docker container. The first mount has the `:ro` attribute to mark it as read only, to make sure that nothing in this mounted directory can be overwritten. The second volume mounted is writable, therefore it's best to mount an empty directory from the host so that nothing of importance gets overwritten. If a directory with previously generated outputs is mounted they will be overwritten if the same operation is performed again.

### Development Build

The development sources are in the `canevas/canevas` directory.

```
cd canevas/canevas # move into the project directory
```

#### Running the software with SBT
The software can be run from within sbt with the following command :

```
sbt run [args]
```

#### Interactively use the framework tools in a Scala console with SBT
The individual functions can be used and run from the interactive Scala console within sbt :

```
sbt
[info] Loading settings for project global-plugins from plugins.sbt ...
...
[info] sbt server started at ...
sbt:Canevas> console
[info] Starting scala interpreter...
Welcome to Scala 2.13.0 (Java HotSpot(TM) 64-Bit Server VM, Java 1.8.0_201).
Type in expressions for evaluation. Or try :help.

scala> 
```

Once the scala interpreter is loaded any function can be called :

```
scala> cnv.SamToolBox.getRegions("GIAB/reads/NIST_30x_downsample/RMNISTHS_30xdownsample.bam")
res22: scala.collection.immutable.Map[String,Int] = HashMap(X -> 155270560, 12 -> 133851895, 8 -> 146364022, 19 -> 59128983, 4 -> 191154276, 13 -> 115169878, 16 -> 90354753, 5 -> 180915260, 10 -> 135534747, 21 -> 48129895, 20 -> 63025520, 2 -> 243199373, 18 -> 78077248, 7 -> 159138663, 3 -> 198022430, 15 -> 102531392, 11 -> 135006516, 9 -> 141213431, Y -> 59373566, 22 -> 51304566, 6 -> 171115067, 1 -> 249250621, 17 -> 81195210, 14 -> 107349540)
```

This is required to play with the assembly related tools.

#### Building the executable
The software executable (Java `.jar` archive) can be built using `sbt` the Scala build tool.

```
cd canevas/canevas # move into the project directory
sbt assembly
```

This will create the executable in the `target/scala-<version>/` directory.