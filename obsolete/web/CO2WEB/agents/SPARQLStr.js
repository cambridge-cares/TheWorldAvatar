/**
  Commented
 * A simple module constructs SPARQL Query strs
 */

    /**
    for list of attributes construct sparql query strs to insert them
    input: list of attributes
    output: query strs
    **/
    var SPARQLStr  = {
        numericalP : "<http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#numericalValue>",
        constructNUpdates : function (attrs) {
        let list = []
        for(let attr of attrs){
            list.push(SPARQLStr.construct('insertdata', this.chevron(attr.uri), 'numerical', {value:attr.value, type: attr.type}))
        }
        return list;
    },


    /**
    for list of attributesconstrict sparql query strs to delete them
    input: list of attributes
    output: query strs
    **/
    constructNDeletes : function (attrs) {
        let list = []
        for(let attr of attrs){
            list.push(SPARQLStr.construct('delete', attr.uri, 'numerical'))
        }
        return list;
    },
        construct(actionType, s, p, o, prefix, template){
            //valid inputs
            this.actionType = actionType.toString().toLowerCase()
            this.s =s;
            this.p = ((p=p.toString())&&p==="numerical")?this.numericalP:p;
            this.o = o ?this.constructO(o):'?o';
            
            
            this.template = template?this.brace(template):'';
            this.prefix = prefix?this.constructPrefix(prefix):''
            return this.combine()
        },
        
        chevron(str) {
            return `<${str}>`
        },
        
        quote(str){
            return `\"${str}\"`
        },
        
        brace(str){
            return `\{${str}\}`
            
        },
        
        
        combine(){
            return this.prefix+this.actionStr()+this.template+this.where+this.brace(this.s+' '+this.p+' '+this.o+'.')
        },
        
              /***
        construct phrase: o in <spo>
       ***/
        constructO : function (o) {
            if(!o){
                return;
            }
            return typeof o ==="string"?o:this.literalO(o)
            
        },
        constructPrefix(prefixMap){
            let str = '';
            for(let ns in prefixMap){
                str+=`PREFIX ${ns}: <${prefixMap[ns]}>\n `
            }
            return str;
        },

              /***
        construct phrase: literal type
       ***/
        literalO({value, type}){
            //oObj{value, type}
            if(!value && !type){
                throw new Error("SPARQLStr: o does not contain value and type")
            }
            //TODO:checktype
            return `\"${value}\"^^<http://www.w3.org/2001/XMLSchema#${type}>`
            
        },
        
    /***
        numerator for sparql update action   
       ***/
        actionStr(){
            let action = this.actionType;
            switch(action){
                case "delete":
                    this.where = ' WHERE '
                    return "DELETE "
                case "insert":
                    this.where = ' WHERE '
                    return "INSERT "
                case "insertdata":
                    this.where = ''
    
                    return "INSERT DATA "
                case "deletedata":
                    this.where = ''
                    return "DELETE DATA "

                default:
                    throw new Error("Non-existing action type for SPARQL")
            }
        },
    
          /***
       query str to construct a coordiniate definition
       ***/
        constructCoordinate({name,url, x,y}){
        let prefix = {
        "coordisystem" :"http://www.theworldavatar.com/ontology/ontocape/upper_level/coordinate_system.owl#",
		"spacetime" :"http://www.theworldavatar.com/ontology/ontocape/supporting_concepts/space_and_time/space_and_time_extended.owl#",
		"pp" :"http://www.theworldavatar.com/ontology/ontoeip/powerplants/PowerPlant.owl#",
		"straightcor" :"http://www.theworldavatar.com/ontology/ontocape/supporting_concepts/space_and_time/space_and_time.owl#",
            "system":"http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#"
        }
        let list = [];
		
		list.push(this.construct("insertdata",  `<${url}#${name}>`, 'a', `pp:PowerPlant` , prefix));
		
		list.push(this.construct("insertdata",  `<${url}#${name}>`, 'spacetime:hasGISCoordinateSystem', `<${url}#CoordinateSystem_of_${name}>` , prefix));
		//list.push(this.construct("insertdata", `<${url}#${name}>`,"<http://www.theworldavatar.com/ontology/ontocape/supporting_concepts/space_and_time/space_and_time_extended.owl#hasGISCoordinateSystem>", `<${url}#CoordinateSystem_of_${name}>`));
		
		list.push(this.construct("insertdata",  `<${url}#CoordinateSystem_of_${name}>`, 'a', `spacetime:GISCoordinateSystem` , prefix));
		//list.push(this.construct("insertdata",  `<${url}#CoordinateSystem_of_${name}>`, 'spacetime:hasProjectedCoordinate_x', '<${url}#x_coordinate_of_${name}>' , prefix));
		//list.push(this.construct("insertdata",  `<${url}#CoordinateSystem_of_${name}>`, 'spacetime:hasProjectedCoordinate_y', '<${url}#y_coordinate_of_${name}>' , prefix));
		
        //x
        list.push(this.construct("insertdata", `<${url}#CoordinateSystem_of_${name}>`,"<http://www.theworldavatar.com/ontology/ontocape/supporting_concepts/space_and_time/space_and_time_extended.owl#hasProjectedCoordinate_x>", `<${url}#x_coordinate_of_${name}>` ));
	   // list.push(this.construct("insertdata", this.chevron(url+"#"+name),"<http://www.theworldavatar.com/ontology/ontocape/upper_level/coordinate_system.owl#hasCoordinate>", `<${prefix.coordisystem}#x_coordinate_of_${name}>` ));
       
        list.push(this.construct("insertdata",  `<${url}#x_coordinate_of_${name}>`, 'a', 'straightcor:StraightCoordinate' , prefix));
            list.push(this.construct("insertdata",  `<${url}#x_coordinate_of_${name}>`, 'system:hasValue', `<${url}#v_xcoordinate_of_${name}>` , prefix));
            list.push(this.construct("insertdata",  `<${url}#v_xcoordinate_of_${name}>`, 'a', `coordisystem:CoordinateValue` , prefix));
            list.push(this.construct("insertdata",  `<${url}#v_xcoordinate_of_${name}>`, 'system:numericalValue', {value:x, type:'float'} , prefix));
    //y
           // list.push(this.construct("insertdata", this.chevron(url+"#"+name),"<http://www.theworldavatar.com/ontology/ontocape/upper_level/coordinate_system.owl#hasCoordinate>", `<${prefix.coordisystem}#y_coordinate_of_${name}>` ));
		   list.push(this.construct("insertdata", `<${url}#CoordinateSystem_of_${name}>`,"<http://www.theworldavatar.com/ontology/ontocape/supporting_concepts/space_and_time/space_and_time_extended.owl#hasProjectedCoordinate_y>", `<${url}#y_coordinate_of_${name}>` ));
		   
            list.push(this.construct("insertdata",  `<${url}#y_coordinate_of_${name}>`, 'a', 'straightcor:StraightCoordinate' , prefix));
            list.push(this.construct("insertdata",  `<${url}#y_coordinate_of_${name}>`, 'system:hasValue', `<${url}#v_ycoordinate_of_${name}>` , prefix));
            list.push(this.construct("insertdata",  `<${url}#v_ycoordinate_of_${name}>`, 'a', `coordisystem:CoordinateValue` , prefix));
            list.push(this.construct("insertdata",  `<${url}#v_ycoordinate_of_${name}>`, 'system:numericalValue', {value:y, type:'float'} , prefix));
            return list;
            
        },
    
        /*
        capacity entity
        **/ 
        constructCapacity({name, url, value}){
            let prefix = {
                "sr" :"http://www.theworldavatar.com/ontology/ontoeip/system_aspects/system_realization.owl#",
                "system":"http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#"
            }
            let list = [];
			//list.push(this.construct("insertdata", `<${url}#${name}>`,"<http://www.theworldavatar.com/ontology/ontoeip/system_aspects/system_realization.owl#designCapacity>", `<${url}#capa_of_${name}>`));
			list.push(this.construct("insertdata", `<${url}#${name}>`,'sr:designCapacity', `<${url}#capa_of_${name}>`, prefix ));
            //list.push(this.construct("insertdata", this.chevron(url+"#"+name),'sr:designCapacity', `<${url}#capa_of_${name}>`, prefix ));
			list.push(this.construct("insertdata", `<${url}#capa_of_${name}>`,'a','sr:DesignCapacity', prefix ));
            list.push(this.construct("insertdata", `<${url}#capa_of_${name}>`,'system:hasValue',`<${url}#v_capa_${name}>`, prefix ));
            list.push(this.construct("insertdata", `<${url}#v_capa_${name}>`,'a',`system:ScalarValue`, prefix ));
            list.push(this.construct("insertdata", `<${url}#v_capa_${name}>`,'system:numericalValue',{value:value, type:'float'} , prefix ));
            
            return list;
            
        },
    
       
       /***
       query str to construct the file definition
       ***/
       constructOwlDef({uri, imports}){
           let prefix = {
               "owl" :"http://www.w3.org/2002/07/owl#"
           }
           let list = [];
           list.push(this.construct("delete", this.chevron("http://www.jparksimulator.com/ppAl/j1.owl"), "?p", "?o"))
           list.push(this.construct("insertdata", this.chevron(uri), 'a', "owl:Ontology", prefix ));
           imports.forEach((importee)=>{
               list.push(this.construct("insertdata", this.chevron(uri), 'owl:imports', this.chevron(importee), prefix ));
    
           })
    
    
        return list;
       }

    
    }
    


module.exports = SPARQLStr