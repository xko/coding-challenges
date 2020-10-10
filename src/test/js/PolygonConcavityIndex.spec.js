require("chai").should()

const solution = require("../../main/js/PolygonConcavityIndex")
function input(arr2d) {
    return arr2d.map(pair => ({x:pair[0],y:pair[1]}) )
}

describe ('the solution', function(){
    it('works on [[0,0],  [0,1],  [2,1],  [1,0],  [2,-1], [0,-1]] ', function(){
        solution(input([[0,0],  [0,1],  [2,1],  [1,0],  [2,-1], [0,-1]])).should.equal(3)
    })

})