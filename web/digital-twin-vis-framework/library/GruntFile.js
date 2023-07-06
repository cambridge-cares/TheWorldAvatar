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
                dest: "./output/dtvf.temp.js"
            }
        },
        uglify: {
            build: {
                files: [
                    {
                        src: "./output/dtvf.temp.js",
                        dest: "./output/dtvf.min.js"
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
                    "./output/dtvf.min.css": ["./src/css/dtvf.css"]
                }
            }
        },
        clean: {
            main: {
                src: [
                    "./output/dtvf.temp.js",
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
    grunt.registerTask("package", ["concat", "uglify", "cssmin", "clean", "copy"]);
}