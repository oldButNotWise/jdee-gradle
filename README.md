Support for using JDEE in Gradle projects.

The lib directory contains a Gradle plugin that defines the "jdee" task.
That task produces a JDEE project file named prj-generated.el that
sets various JDEE path variables based on the workings of Gradle.
Note that you may need to run the "assemble" task before "jdee"
to ensure that Gradle has full knowledge of all the referenced libraries.

The lisp directory contains elisp code that sets up Gradle as the build tool for JDEE.

The samples directory contains to example skeleton projects that use all this.
