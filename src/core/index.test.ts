const hello = (str: string) => 'Hello Jest!!';

describe('hello', () => {
  it('hello("jest") to be "Hello Jest!!"', () => {
    expect(hello('Jest')).toBe('Hello Jest!!');
  });
  it('hello("jest") not to be "Hello fukumasuya!!"', () => {
    expect(hello('Jest')).not.toBe('Hello fukumasuya!!');
  });
});
