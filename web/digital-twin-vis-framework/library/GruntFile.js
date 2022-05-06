module.exports = function (grunt) {
    // Task Configuration
    grunt.initConfig({
        concat: {
            dist: {
                src: [
                    "./output/**/*.js"
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
                    "./output/dtvf.min.css": ["./src/css/dtvf.css"],
                    "./output/controls.css": ["./src/css/controls.css"]
                }
            }
        },
        copy: {
            main: {
                expand: true,
                flatten: true,
                src: "./src/html/*",
                dest: "./output/html/"
            }
        }
    });

    // Load Plugins
    grunt.loadNpmTasks("grunt-contrib-concat");
    grunt.loadNpmTasks("grunt-contrib-uglify");
    grunt.loadNpmTasks("grunt-contrib-cssmin");
    grunt.loadNpmTasks("grunt-contrib-copy");

    // Register Tasks
    grunt.registerTask("package", ["concat", "uglify", "cssmin", "copy"]);
}