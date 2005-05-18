typedef char bool;

const bool b;

union charOrInt {
  struct {
    bool isChar;
    char x;
  };
  struct {
    bool isChar;
    int  x;
  };
};

