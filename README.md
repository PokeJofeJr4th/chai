# Chai

Chai is a Rust-inspired language that compiles to JVM bytecode. It aims to provide a concise, easily-understandable syntax that delivers abstractions like tuples

# Examples

The following examples give some insight into how Chai works, including examples of decompiled output

### Fizzbuzz

A classic coding example, this fizzbuzz implementation shows a dense set of chai features:

- python-style for loops and printing
- java-style imports and types
- rust-inspired if statements
- access to builtin java classes (`Integer.parseInt`)

```rs
// file: Example.chai
import option.*;

fn fizzbuzz(int num) {
    for int i in range(0, num) {
        if i % 15 == 0 {
            print("FizzBuzz");
        } else if i % 5 == 0 {
            print("Fizz");
        } else if i % 3 == 0 {
            print("Buzz");
        } else {
            print(i);
        }
    }
}

fn main(String[] args) {
    fizzbuzz(Integer.parseInt(args[0]));
}
```

Here's what this code looks like compiled by Chai and then decompiled to Java by Fernflower:

```java
// Source code is decompiled from a .class file using FernFlower decompiler.
public class Example {
   public static void fizzbuzz(int var0) {
      for(int var1 = 0; var1 < var0; ++var1) {
         if (var1 % 15 == 0) {
            System.out.println("FizzBuzz");
         } else if (var1 % 5 == 0) {
            System.out.println("Fizz");
         } else if (var1 % 3 == 0) {
            System.out.println("Buzz");
         } else {
            System.out.println(var1);
         }
      }

   }

   public static void main(String[] var0) {
      fizzbuzz(Integer.parseInt(var0[0]));
   }
}
```

Attention should be drawn to Chai's ability to compile the `for ... in range(...)` to a simple loop and the expansion of the print method.

### Math

These two methods show off a few other features of chai:

- Ternary operator
- Built-in alias methods for optionals
- Tuples are first-class types

```rs
// file: Example.chai
import java.util.Optional;
import option.*;

class Math {
    fn div(float a, float b) -> Optional<float> {
        b == 0.0 ? none() : some(a / b)
    }

    fn div(int a, int b) -> (int, int) {
        (a / b, a % b)
    }
}
```

Here it is compiled and decompiled again:

```java
// Source code is decompiled from a .class file using FernFlower decompiler.
import java.util.Optional;

public class Example$Math {
   public static Optional div(float var0, float var1) {
      return var1 == 0.0F ? Optional.empty() : Optional.of(var0 / var1);
   }

   public static int[] div(int var0, int var1) {
      return new int[]{var0 / var1, var0 % var1};
   }
}
```

Notice a few things:

- Expansion of `none()` and `some(...)` methods for the Optional type
- The tuple type `(int, int)` is replaced at runtime with `int[]` and constructed accordingly
- The compiler will select the most specific type it can to fit all of the tuple elements into the same array
