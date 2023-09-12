; if (typeof Cesium !== 'undefined') (function (Cesium) {

    /**
     * 通过重写模型每一帧渲染着色逻辑注入自定义着色器
     * 性能以及通用性较差,建议单独写一套3dtiles加载逻辑，自定义着色程序
     */
    Cesium.TILE_EFFECT_STATE = true;
    Cesium.TILE_FS_BODY = ` float stc_pl = fract(czm_frameNumber / 120.0) * 3.14159265 * 2.0;
                float stc_sd = v_stcVertex.z / 60.0 + sin(stc_pl) * 0.1;
                gl_FragColor *= vec4(stc_sd, stc_sd, stc_sd, 1.0);
                float stc_a13 = fract(czm_frameNumber / 360.0);
                float stc_h = clamp(v_stcVertex.z / 450.0, 0.0, 1.0);
                stc_a13 = abs(stc_a13 - 0.5) * 2.0;
                float stc_diff = step(0.005, abs(stc_h - stc_a13));
                gl_FragColor.rgb += gl_FragColor.rgb * (1.0 - stc_diff);`;

    function install() {

        Cesium.ModelUtility.updateForwardAxis = function (model) {
            this._model = model;
            var cachedSourceVersion = model.gltf.extras.sourceVersion;

            if (
                (Cesium.defined(cachedSourceVersion) && cachedSourceVersion !== "2.0") ||
                Cesium.ModelUtility.getAssetVersion(model.gltf) !== "2.0"
            ) {
                model._gltfForwardAxis = Cesium.Axis.X;
            }
        };

        Cesium.ModelUtility.modifyFragmentShaderForLogDepth = function (shader) {
            let state = false;
            if (Cesium.TILE_EFFECT_STATE && this._model && this._model._resource._url.indexOf('b3dm') !== -1) state = true;
            shader = Cesium.ShaderSource.replaceMain(shader, "czm_depth_main");

            if (state) {
                shader += `
                varying vec3 v_stcVertex;
                void main(){
                    czm_depth_main();
                    `+ Cesium.TILE_FS_BODY + `
                    czm_writeLogDepth();
                }
                `;
            } else {
                shader += `
                    varying vec3 v_stcVertex;
                    void main(){
                        czm_depth_main();
                        czm_writeLogDepth();
                    }
                    `;
            }
            return shader;
        };

        Cesium.ModelUtility.modifyVertexShaderForLogDepth = function (
            shader,
            toClipCoordinatesGLSL
        ) {
            shader = Cesium.ShaderSource.replaceMain(shader, "czm_depth_main");
            shader +=
                "\n" +
                "varying vec3 v_stcVertex;\n" +
                "void main() \n" +
                "{ \n" +
                "    czm_depth_main(); \n" +
                "    v_stcVertex = a_position;\n" +
                "    czm_vertexLogDepth(" +
                toClipCoordinatesGLSL +
                "); \n" +
                "} \n";
            return shader;
        };
    }
    install();
    let a = 'background: #606060; color: #fff; border-radius: 3px 0 0 3px;'
    let b = 'background: #1475B2; color: #fff; border-radius: 0 3px 3px 0;'
    console.log(`%c web3d/webgis相关工作推荐邮箱 : %c zhangticcc@163.com `, a, b)
    console.log(`https://github.com/zhangti0708 %c =,=给个广告位`, b)

})(Cesium)