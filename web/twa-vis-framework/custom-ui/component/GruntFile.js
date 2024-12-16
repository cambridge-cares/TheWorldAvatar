module.exports = function (grunt) {
    // Task Configuration
    grunt.initConfig({
        concat: {
            dist: {
                src: [
                    "./output/ts/dynamic_component.js",
                    "./output/ts/utils.js",
                    "./output/ts/*/*.js"
                ],
                dest: "./output/component.temp.js"
            }
        },
        uglify: {
            build: {
                files: [
                    {
                        src: "./output/component.temp.js",
                        dest: "./output/component.min.js"
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
                    "./output/component.min.css": ["./src/css/component.css"]
                }
            }
        },
        clean: {
            main: {
                src: [
                    "./output/component.temp.js",
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
    grunt.registerTask("package", ["concat", "uglify", "cssmin", "clean", "copy"]);
}