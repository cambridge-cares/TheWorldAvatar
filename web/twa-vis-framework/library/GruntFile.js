module.exports = function (grunt) {
    // Task Configuration
    grunt.initConfig({
        concat: {
            dist: {
                src: [
                    "./output/ts/io/*.js",
                    "./output/ts/*.js",
                    "./output/ts/ui/*.js",
                    "./output/ts/mapbox/*.js",
                    "./output/ts/cesium/*.js"
                ],
                dest: "./output/twa-vf.min.js"
            }
        },
        uglify: {
            build: {
                files: [
                    {
                        src: "./output/twa-vf.temp.js",
                        dest: "./output/twa-vf.min.js"
                    }
                ]
            }
        },
        cssmin: {
            options: {
                mergeIntoShorthands: false,
                roundingPrecision: -1
            },
            target: {
                files: {
                    "./output/twa-vf.min.css": ["./src/css/twa-vf.css"]
                }
            }
        },
        clean: {
            main: {
                src: [
                    "./output/twa-vf.temp.js",
                    "./output/ts/**"
                ]
            }
        },
        copy: {
            main: {
                files : [
                    {
                        expand: true,
                        cwd: "./src/html",
                        src: "**",
                        dest: "./output/"
                    },
                    {
                        expand: true,
                        cwd: '.',
                        src: ["VERSION"],
                        dest: "./output/"
                    }
                ]
            }
        }
    });

    // Load Plugins
    grunt.loadNpmTasks("grunt-contrib-concat");
    grunt.loadNpmTasks("grunt-contrib-uglify");
    grunt.loadNpmTasks("grunt-contrib-cssmin");
    grunt.loadNpmTasks("grunt-contrib-clean");
    grunt.loadNpmTasks("grunt-contrib-copy");

    // Register Tasks
	// To build an unminified version of the code, temporarily remove the "uglify" task here.
    //grunt.registerTask("package", ["concat", "uglify", "cssmin", "clean", "copy"]);
    grunt.registerTask("package", ["concat", "cssmin", "clean", "copy"]);
}