pragma solidity ^0.4.0;

contract Test{

    string[] public infos = ['hello', 'fuck'];
    mapping(address => uint) public score_map;
    mapping(address => uint) public number_map;



    function get_score(address agent_id) public view returns(uint[2]) {
        return [score_map[agent_id],number_map[agent_id]];
    }

    function update_score(address agent_id, uint[3] score_matrix) public returns(uint[2]) {
        uint time = score_matrix[0];
        uint code = score_matrix[1];
        uint covr = score_matrix[2];
        uint score = calculate_score(time,code,covr);
        number_map[agent_id] = number_map[agent_id] + 1;
        score_map[agent_id] = score_map[agent_id] + score;
        return [score,number_map[agent_id]];
    }

    function getInfoLength() public view returns(uint) {
        return infos.length;
    }

    function percent(uint numerator, uint denominator, uint precision)  public
    pure  returns(uint quotient) {
        uint _numerator  = numerator * 10 ** (precision+1);
        uint _quotient =  ((_numerator / denominator) + 5) / 10;
        return ( _quotient);
    }

    function calculate_score(uint time, uint code, uint coverage) public pure returns(uint score)
    {
        uint time_component = percent(5000,time,5);
        uint code_component = 0;
        if(code == 404){
            code_component = 0;
        }
        if(code == 200){
            code_component = 1;
        }

        uint covr_component = coverage * 5000;
        return time_component + code_component * 5000 + covr_component;
    }

}