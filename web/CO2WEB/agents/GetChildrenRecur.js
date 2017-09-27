/**
 * Created by Shaocong on 9/26/2017.
 * Rewrite FileConnection function in endpooint query style
 */

var log4js = require('log4js');
var logger = log4js.getLogger();
logger.level = 'debug';
//import
var path = require('path');
var libxmljs = require("libxmljs");
var proj4 = require('proj4');


var async = require('async');
var readdirp = require('readdirp');
var fs = require('graceful-fs');
var util = require('util');
var config = require('../config.js');
//var folderLocation = config.root;
let request = require('request');



var GetChildrenRecur = function (options, callback) {

    this.mergeOptions(options);
    this.prefix =[];
    this.QCon = new Map();
};


GetChildrenRecur.prototype = {

    mergeOptions: function (options) {
        /*settings***********************/
        const defaultQueryContent = ["children", "myUri"];
        //TODO: chidrena and myUi are must, do a merge function properly maybe
        this.queryContent = options.queryContent || defaultQueryContent ;
        //["children", "myUri", "geoCoord", "type","service","import"];
        this.topnodeLocation = options.topnode;
    },
    constructPrefix : function () {
        let prefixStr = "";
        this.prefix.forEach(function (prefix) {
            prefixStr+="PREFIX "+prefix.ns+": <"+prefix.uri+">\n";
      });
        return prefixStr;
    },
    /**
     * construct Query Condition Map
     */
    constructQConditionMap : function () {

    },

    /**
     * TODO: test each of this, put into QC Map
     * @returns {string}
     */
    constructQueryStr: function () {
      //let prefixStr  = this.constructPrefix()
        let selectStr= "select distinct";
        let whereStr = "where {\n";
      this.queryContent.forEach(function (queryItem) {
          selectStr+="?"+queryItem+"  ";
          whereStr+=this.QCon[queryItem];
      })
        selectStr+="\n";
        whereStr+="}";
        return this.constructPrefix()+selectStr+whereStr;
    },

    getChildrenQueryStr : function () {
        this.prefix = [];
        this.qa = `
   select distinct  ?children
    where {
    ?x industrialPark:hasIRI|system:hasIRI ?children.    
    }
`;
    },

    getmyUriQueryStr : function () {
        this.qa = `
   select distinct  ?myUri
    where {
    ?x Service:hasUrl ?myUri.    
    }
`;
    },

    getGeoCoordQueryStr : function () {

    },

   getTypeQueryStr : function () {

   },

    getServiceQueryStr : function () {

    },

    getImportQueryStr : function () {

    },

    queryStartTop :function () {

    },

    queryRecur : function () {

    }
    
};