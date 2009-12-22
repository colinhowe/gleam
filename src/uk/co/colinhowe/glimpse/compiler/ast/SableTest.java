package uk.co.colinhowe.glimpse.compiler.ast;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.PushbackReader;

import uk.co.colinhowe.glimpse.compiler.GlimpseCompiler;
import uk.co.colinhowe.glimpse.compiler.lexer.Lexer;
import uk.co.colinhowe.glimpse.compiler.lexer.LexerException;
import uk.co.colinhowe.glimpse.compiler.node.Start;
import uk.co.colinhowe.glimpse.compiler.parser.Parser;
import uk.co.colinhowe.glimpse.compiler.parser.ParserException;

public class SableTest {

  
 
  
  
  public static void main(String[] args) throws ParserException, LexerException, IOException {


    InputStream stream = SableTest.class.getResourceAsStream("/uk/co/colinhowe/glimpse/examples/basic-string.glimpse");
    
    stream = SableTest.class.getResourceAsStream("/uk/co/colinhowe/glimpse/examples/basic-string.glimpse");
    
    // create lexer
//    Lexer lexer = new Lexer (new PushbackReader(new BufferedReader(new FileReader(args[0])), 1024));
    Lexer lexer = new Lexer (new PushbackReader(new BufferedReader(new InputStreamReader(stream))));
//    Token token;
//    while ((token = lexer.next()) != null) {
//      if (token instanceof EOF) {
//        break;
//      }
//      System.out.println(token.getText());
//    }
    
    // parser program
    Parser parser = new Parser(lexer);

    Start ast = parser.parse();

    System.out.println(ast);
    
    System.out.println("==== Compiling ====");
    new GlimpseCompiler().compile("basic-string", ast);
  }
}
