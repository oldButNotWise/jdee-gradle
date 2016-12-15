## Overview

Support for using JDEE in Gradle projects.

The lib directory contains a Gradle plugin that defines the "jdee" task.
That task produces a JDEE project file named prj-generated.el that
sets various JDEE path variables based on the workings of Gradle.
Note that you may need to run the "assemble" task before "jdee"
to ensure that Gradle has full knowledge of all the referenced libraries.

The lisp directory contains elisp code that sets up Gradle as the build tool for JDEE.

The samples directory contains to example skeleton projects that use all this.


## Requirements

  * [gradle-el](https://github.com/vhallac/gradle-el)
  * [jdee](https://github.com/jdee-emacs/jdee)


## Usage

See the [single project](samples/single-project) or [multi-project](samples/multi-project)
directories for examples.

1. Copy the file [jdee-gradle](lisp/jdee-gradle.el) to a directory on your Emacs load-path.
   You probably want to byte-compile it, but that isn't necessary.

1. Put the Gradle source file [jdee.gradle](lib/jdee.gradle) somewhere.

1. Edit the project build.gradle file to include that source file, for example by adding the line

        apply from: "path/to/file/jdee.gradle"

1. Modify or create a JDEE project file at the root of your Gradle project to include the lines:

        (require 'jdee-gradle)
        
        (jdee-gradle-set-project "project-name")

1. Execute the jdee task to create the prj-generated files:

        $ gradlew assemble jdee

   You will need to re-run the jdee task occasionally to keep the produced file up-to-date.

