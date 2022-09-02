module.exports = function (grunt) {
    // Task Configuration
    grunt.initConfig({
        concat: {
            dist: {
                src: [
                    "./output/ts/**/*.js"
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
        }
    });

    // Load Plugins
    grunt.loadNpmTasks("grunt-contrib-concat");
    grunt.loadNpmTasks("grunt-contrib-uglify");
    grunt.loadNpmTasks("grunt-contrib-cssmin");
    grunt.loadNpmTasks("grunt-contrib-copy");
    grunt.loadNpmTasks("grunt-contrib-clean");

    // Register Tasks
    grunt.registerTask("package", ["concat", "uglify", "cssmin", "clean"]);
}