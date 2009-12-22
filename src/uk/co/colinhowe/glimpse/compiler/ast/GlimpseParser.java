// $ANTLR 3.2 Sep 23, 2009 12:02:23 G:\\workspace\\grammar-sandbox\\src\\uk\\co\\colinhowe\\glimpse\\compiler\\ast\\Glimpse.g 2009-12-05 09:46:31

  package uk.co.colinhowe.glimpse.compiler.ast;


import org.antlr.runtime.*;
import java.util.Stack;
import java.util.List;
import java.util.ArrayList;


import org.antlr.runtime.tree.*;

public class GlimpseParser extends Parser {
    public static final String[] tokenNames = new String[] {
        "<invalid>", "<EOR>", "<DOWN>", "<UP>", "NODE", "ID", "STRING_LITERAL", "WHITESPACE", "EscapeSequence", "STRINGLITERAL", "':'"
    };
    public static final int STRING_LITERAL=6;
    public static final int NODE=4;
    public static final int STRINGLITERAL=9;
    public static final int T__10=10;
    public static final int WHITESPACE=7;
    public static final int ID=5;
    public static final int EOF=-1;
    public static final int EscapeSequence=8;

    // delegates
    // delegators


        public GlimpseParser(TokenStream input) {
            this(input, new RecognizerSharedState());
        }
        public GlimpseParser(TokenStream input, RecognizerSharedState state) {
            super(input, state);
             
        }
        
    protected TreeAdaptor adaptor = new CommonTreeAdaptor();

    public void setTreeAdaptor(TreeAdaptor adaptor) {
        this.adaptor = adaptor;
    }
    public TreeAdaptor getTreeAdaptor() {
        return adaptor;
    }

    public String[] getTokenNames() { return GlimpseParser.tokenNames; }
    public String getGrammarFileName() { return "G:\\workspace\\grammar-sandbox\\src\\uk\\co\\colinhowe\\glimpse\\compiler\\ast\\Glimpse.g"; }


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


    public static class view_return extends ParserRuleReturnScope {
        Object tree;
        public Object getTree() { return tree; }
    };

    // $ANTLR start "view"
    // G:\\workspace\\grammar-sandbox\\src\\uk\\co\\colinhowe\\glimpse\\compiler\\ast\\Glimpse.g:36:1: view : ( node )* ;
    public final GlimpseParser.view_return view() throws RecognitionException {
        GlimpseParser.view_return retval = new GlimpseParser.view_return();
        retval.start = input.LT(1);

        Object root_0 = null;

        GlimpseParser.node_return node1 = null;



        try {
            // G:\\workspace\\grammar-sandbox\\src\\uk\\co\\colinhowe\\glimpse\\compiler\\ast\\Glimpse.g:36:6: ( ( node )* )
            // G:\\workspace\\grammar-sandbox\\src\\uk\\co\\colinhowe\\glimpse\\compiler\\ast\\Glimpse.g:36:8: ( node )*
            {
            root_0 = (Object)adaptor.nil();

            // G:\\workspace\\grammar-sandbox\\src\\uk\\co\\colinhowe\\glimpse\\compiler\\ast\\Glimpse.g:36:8: ( node )*
            loop1:
            do {
                int alt1=2;
                int LA1_0 = input.LA(1);

                if ( (LA1_0==NODE) ) {
                    alt1=1;
                }


                switch (alt1) {
            	case 1 :
            	    // G:\\workspace\\grammar-sandbox\\src\\uk\\co\\colinhowe\\glimpse\\compiler\\ast\\Glimpse.g:36:8: node
            	    {
            	    pushFollow(FOLLOW_node_in_view69);
            	    node1=node();

            	    state._fsp--;

            	    adaptor.addChild(root_0, node1.getTree());

            	    }
            	    break;

            	default :
            	    break loop1;
                }
            } while (true);


            }

            retval.stop = input.LT(-1);

            retval.tree = (Object)adaptor.rulePostProcessing(root_0);
            adaptor.setTokenBoundaries(retval.tree, retval.start, retval.stop);

        }
        catch (RecognitionException re) {
            reportError(re);
            recover(input,re);
    	retval.tree = (Object)adaptor.errorNode(input, retval.start, input.LT(-1), re);

        }
        finally {
        }
        return retval;
    }
    // $ANTLR end "view"

    public static class node_return extends ParserRuleReturnScope {
        Object tree;
        public Object getTree() { return tree; }
    };

    // $ANTLR start "node"
    // G:\\workspace\\grammar-sandbox\\src\\uk\\co\\colinhowe\\glimpse\\compiler\\ast\\Glimpse.g:38:1: node : NODE ':' id= ID (text= STRING_LITERAL )? -> NODE[$id] ;
    public final GlimpseParser.node_return node() throws RecognitionException {
        GlimpseParser.node_return retval = new GlimpseParser.node_return();
        retval.start = input.LT(1);

        Object root_0 = null;

        Token id=null;
        Token text=null;
        Token NODE2=null;
        Token char_literal3=null;

        Object id_tree=null;
        Object text_tree=null;
        Object NODE2_tree=null;
        Object char_literal3_tree=null;
        RewriteRuleTokenStream stream_10=new RewriteRuleTokenStream(adaptor,"token 10");
        RewriteRuleTokenStream stream_STRING_LITERAL=new RewriteRuleTokenStream(adaptor,"token STRING_LITERAL");
        RewriteRuleTokenStream stream_NODE=new RewriteRuleTokenStream(adaptor,"token NODE");
        RewriteRuleTokenStream stream_ID=new RewriteRuleTokenStream(adaptor,"token ID");

        try {
            // G:\\workspace\\grammar-sandbox\\src\\uk\\co\\colinhowe\\glimpse\\compiler\\ast\\Glimpse.g:38:6: ( NODE ':' id= ID (text= STRING_LITERAL )? -> NODE[$id] )
            // G:\\workspace\\grammar-sandbox\\src\\uk\\co\\colinhowe\\glimpse\\compiler\\ast\\Glimpse.g:38:8: NODE ':' id= ID (text= STRING_LITERAL )?
            {
            NODE2=(Token)match(input,NODE,FOLLOW_NODE_in_node78);  
            stream_NODE.add(NODE2);

            char_literal3=(Token)match(input,10,FOLLOW_10_in_node80);  
            stream_10.add(char_literal3);

            id=(Token)match(input,ID,FOLLOW_ID_in_node84);  
            stream_ID.add(id);

            // G:\\workspace\\grammar-sandbox\\src\\uk\\co\\colinhowe\\glimpse\\compiler\\ast\\Glimpse.g:38:27: (text= STRING_LITERAL )?
            int alt2=2;
            int LA2_0 = input.LA(1);

            if ( (LA2_0==STRING_LITERAL) ) {
                alt2=1;
            }
            switch (alt2) {
                case 1 :
                    // G:\\workspace\\grammar-sandbox\\src\\uk\\co\\colinhowe\\glimpse\\compiler\\ast\\Glimpse.g:38:27: text= STRING_LITERAL
                    {
                    text=(Token)match(input,STRING_LITERAL,FOLLOW_STRING_LITERAL_in_node88);  
                    stream_STRING_LITERAL.add(text);


                    }
                    break;

            }



            // AST REWRITE
            // elements: NODE
            // token labels: 
            // rule labels: retval
            // token list labels: 
            // rule list labels: 
            // wildcard labels: 
            retval.tree = root_0;
            RewriteRuleSubtreeStream stream_retval=new RewriteRuleSubtreeStream(adaptor,"rule retval",retval!=null?retval.tree:null);

            root_0 = (Object)adaptor.nil();
            // 38:44: -> NODE[$id]
            {
                adaptor.addChild(root_0, new V(NODE, id));

            }

            retval.tree = root_0;
            }

            retval.stop = input.LT(-1);

            retval.tree = (Object)adaptor.rulePostProcessing(root_0);
            adaptor.setTokenBoundaries(retval.tree, retval.start, retval.stop);

        }
        catch (RecognitionException re) {
            reportError(re);
            recover(input,re);
    	retval.tree = (Object)adaptor.errorNode(input, retval.start, input.LT(-1), re);

        }
        finally {
        }
        return retval;
    }
    // $ANTLR end "node"

    // Delegated rules


 

    public static final BitSet FOLLOW_node_in_view69 = new BitSet(new long[]{0x0000000000000012L});
    public static final BitSet FOLLOW_NODE_in_node78 = new BitSet(new long[]{0x0000000000000400L});
    public static final BitSet FOLLOW_10_in_node80 = new BitSet(new long[]{0x0000000000000020L});
    public static final BitSet FOLLOW_ID_in_node84 = new BitSet(new long[]{0x0000000000000042L});
    public static final BitSet FOLLOW_STRING_LITERAL_in_node88 = new BitSet(new long[]{0x0000000000000002L});

}