grammar Glimpse;


options {
  language = Java;
  output = AST;
}


tokens {
  NODE='node';
}

@members {
static class V extends CommonTree {
  String id;

  public V(int ttype, Token t) { 
    token=t;
    id = token.getText(); 
  }
  public String toString() {
    return (token!=null?token.getText():"")+"<V>;";
  }
}
}

@header {
  package uk.co.colinhowe.glimpse.compiler.ast;
}

@lexer::header {
  package uk.co.colinhowe.glimpse.compiler.ast; 
} 

view : node*;

node : NODE ':' id=ID text=STRING_LITERAL? -> NODE<V>[$id] ;

ID  : ('a'..'z' | 'A'..'Z')+ ;

WHITESPACE : ( '\t' | ' ' | '\r' | '\n'| '\u000C' )+  { $channel = HIDDEN; } ;

STRINGLITERAL :   '"' 
        (   EscapeSequence
        |   ~( '\\' | '"' | '\r' | '\n' )        
        )* 
        '"' ;

fragment EscapeSequence 
    :   '\\' (
                 'b' 
             |   't' 
             |   'n' 
             |   'f' 
             |   'r' 
             |   '\"' 
             |   '\'' 
             |   '\\' 
             |       
                 ('0'..'3') ('0'..'7') ('0'..'7')
             |       
                 ('0'..'7') ('0'..'7') 
             |       
                 ('0'..'7')
             )          
;     
        