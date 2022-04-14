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
                        src: './output/dtvf.temp.js',
                        dest: './output/dtvf.min.js'
                    }
                ]
            }
        },
        cssmin: {
            css: {
                src: "./src/css/dtvf.css",
                dest: "./output/dtvf.min.css"
            }
        }
    });

    // Load Plugins
    grunt.loadNpmTasks('grunt-contrib-concat');
    grunt.loadNpmTasks('grunt-contrib-uglify');
    grunt.loadNpmTasks('grunt-contrib-cssmin');

    // Register Tasks
    grunt.registerTask('package', ['concat', 'uglify', 'cssmin']);
}