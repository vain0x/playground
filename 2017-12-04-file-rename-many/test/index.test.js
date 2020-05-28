const chai = require("chai");

const should = chai.should();

describe("Index", function () {
    it("pass", function () {
        (1).should.equal(1);
        should.throw(() => {
            throw new Error("Ok");
        });
    });
});
