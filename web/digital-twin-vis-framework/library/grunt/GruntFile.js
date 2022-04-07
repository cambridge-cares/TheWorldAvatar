module.exports = function (grunt) {
    // Task Configuration
    grunt.initConfig({
        concat: {
            dist: {
                src: [
                    "../src/js/control_handler.js",
                    "../src/js/data_registry.js",
                    "../src/js/icon_handler.js",
                    "../src/js/interaction_handler.js",
                    "../src/js/json_tree.js",
                    "../src/js/layer_handler.js",
                    "../src/js/manager.js",
                    "../src/js/panel_handler.js",
                    "../src/js/source_handler.js",
                    "../src/js/timeseries_handler.js"
                ],
                dest: "./dtvf.temp.js"
            }
        },
        uglify: {
            build: {
                files: [
                    {
                        src: './dtvf.temp.js',
                        dest: '../output/dtvf.min.js'
                    }
                ]
            }
        },
        cssmin: {
            css: {
                src: "../src/css/dtvf.css",
                dest: "../output/dtvf.min.css"
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