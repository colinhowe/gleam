// $ANTLR 3.2 Sep 23, 2009 12:02:23 G:\\workspace\\grammar-sandbox\\src\\uk\\co\\colinhowe\\glimpse\\compiler\\ast\\Glimpse.g 2009-12-05 09:46:32

  package uk.co.colinhowe.glimpse.compiler.ast; 


import org.antlr.runtime.*;
import java.util.Stack;
import java.util.List;
import java.util.ArrayList;

public class GlimpseLexer extends Lexer {
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

    public GlimpseLexer() {;} 
    public GlimpseLexer(CharStream input) {
        this(input, new RecognizerSharedState());
    }
    public GlimpseLexer(CharStream input, RecognizerSharedState state) {
        super(input,state);

    }
    public String getGrammarFileName() { return "G:\\workspace\\grammar-sandbox\\src\\uk\\co\\colinhowe\\glimpse\\compiler\\ast\\Glimpse.g"; }

    // $ANTLR start "NODE"
    public final void mNODE() throws RecognitionException {
        try {
            int _type = NODE;
            int _channel = DEFAULT_TOKEN_CHANNEL;
            // G:\\workspace\\grammar-sandbox\\src\\uk\\co\\colinhowe\\glimpse\\compiler\\ast\\Glimpse.g:11:6: ( 'node' )
            // G:\\workspace\\grammar-sandbox\\src\\uk\\co\\colinhowe\\glimpse\\compiler\\ast\\Glimpse.g:11:8: 'node'
            {
            match("node"); 


            }

            state.type = _type;
            state.channel = _channel;
        }
        finally {
        }
    }
    // $ANTLR end "NODE"

    // $ANTLR start "T__10"
    public final void mT__10() throws RecognitionException {
        try {
            int _type = T__10;
            int _channel = DEFAULT_TOKEN_CHANNEL;
            // G:\\workspace\\grammar-sandbox\\src\\uk\\co\\colinhowe\\glimpse\\compiler\\ast\\Glimpse.g:12:7: ( ':' )
            // G:\\workspace\\grammar-sandbox\\src\\uk\\co\\colinhowe\\glimpse\\compiler\\ast\\Glimpse.g:12:9: ':'
            {
            match(':'); 

            }

            state.type = _type;
            state.channel = _channel;
        }
        finally {
        }
    }
    // $ANTLR end "T__10"

    // $ANTLR start "ID"
    public final void mID() throws RecognitionException {
        try {
            int _type = ID;
            int _channel = DEFAULT_TOKEN_CHANNEL;
            // G:\\workspace\\grammar-sandbox\\src\\uk\\co\\colinhowe\\glimpse\\compiler\\ast\\Glimpse.g:40:5: ( ( 'a' .. 'z' | 'A' .. 'Z' )+ )
            // G:\\workspace\\grammar-sandbox\\src\\uk\\co\\colinhowe\\glimpse\\compiler\\ast\\Glimpse.g:40:7: ( 'a' .. 'z' | 'A' .. 'Z' )+
            {
            // G:\\workspace\\grammar-sandbox\\src\\uk\\co\\colinhowe\\glimpse\\compiler\\ast\\Glimpse.g:40:7: ( 'a' .. 'z' | 'A' .. 'Z' )+
            int cnt1=0;
            loop1:
            do {
                int alt1=2;
                int LA1_0 = input.LA(1);

                if ( ((LA1_0>='A' && LA1_0<='Z')||(LA1_0>='a' && LA1_0<='z')) ) {
                    alt1=1;
                }


                switch (alt1) {
            	case 1 :
            	    // G:\\workspace\\grammar-sandbox\\src\\uk\\co\\colinhowe\\glimpse\\compiler\\ast\\Glimpse.g:
            	    {
            	    if ( (input.LA(1)>='A' && input.LA(1)<='Z')||(input.LA(1)>='a' && input.LA(1)<='z') ) {
            	        input.consume();

            	    }
            	    else {
            	        MismatchedSetException mse = new MismatchedSetException(null,input);
            	        recover(mse);
            	        throw mse;}


            	    }
            	    break;

            	default :
            	    if ( cnt1 >= 1 ) break loop1;
                        EarlyExitException eee =
                            new EarlyExitException(1, input);
                        throw eee;
                }
                cnt1++;
            } while (true);


            }

            state.type = _type;
            state.channel = _channel;
        }
        finally {
        }
    }
    // $ANTLR end "ID"

    // $ANTLR start "WHITESPACE"
    public final void mWHITESPACE() throws RecognitionException {
        try {
            int _type = WHITESPACE;
            int _channel = DEFAULT_TOKEN_CHANNEL;
            // G:\\workspace\\grammar-sandbox\\src\\uk\\co\\colinhowe\\glimpse\\compiler\\ast\\Glimpse.g:42:12: ( ( '\\t' | ' ' | '\\r' | '\\n' | '\\u000C' )+ )
            // G:\\workspace\\grammar-sandbox\\src\\uk\\co\\colinhowe\\glimpse\\compiler\\ast\\Glimpse.g:42:14: ( '\\t' | ' ' | '\\r' | '\\n' | '\\u000C' )+
            {
            // G:\\workspace\\grammar-sandbox\\src\\uk\\co\\colinhowe\\glimpse\\compiler\\ast\\Glimpse.g:42:14: ( '\\t' | ' ' | '\\r' | '\\n' | '\\u000C' )+
            int cnt2=0;
            loop2:
            do {
                int alt2=2;
                int LA2_0 = input.LA(1);

                if ( ((LA2_0>='\t' && LA2_0<='\n')||(LA2_0>='\f' && LA2_0<='\r')||LA2_0==' ') ) {
                    alt2=1;
                }


                switch (alt2) {
            	case 1 :
            	    // G:\\workspace\\grammar-sandbox\\src\\uk\\co\\colinhowe\\glimpse\\compiler\\ast\\Glimpse.g:
            	    {
            	    if ( (input.LA(1)>='\t' && input.LA(1)<='\n')||(input.LA(1)>='\f' && input.LA(1)<='\r')||input.LA(1)==' ' ) {
            	        input.consume();

            	    }
            	    else {
            	        MismatchedSetException mse = new MismatchedSetException(null,input);
            	        recover(mse);
            	        throw mse;}


            	    }
            	    break;

            	default :
            	    if ( cnt2 >= 1 ) break loop2;
                        EarlyExitException eee =
                            new EarlyExitException(2, input);
                        throw eee;
                }
                cnt2++;
            } while (true);

             _channel = HIDDEN; 

            }

            state.type = _type;
            state.channel = _channel;
        }
        finally {
        }
    }
    // $ANTLR end "WHITESPACE"

    // $ANTLR start "STRINGLITERAL"
    public final void mSTRINGLITERAL() throws RecognitionException {
        try {
            int _type = STRINGLITERAL;
            int _channel = DEFAULT_TOKEN_CHANNEL;
            // G:\\workspace\\grammar-sandbox\\src\\uk\\co\\colinhowe\\glimpse\\compiler\\ast\\Glimpse.g:44:15: ( '\"' ( EscapeSequence | ~ ( '\\\\' | '\"' | '\\r' | '\\n' ) )* '\"' )
            // G:\\workspace\\grammar-sandbox\\src\\uk\\co\\colinhowe\\glimpse\\compiler\\ast\\Glimpse.g:44:19: '\"' ( EscapeSequence | ~ ( '\\\\' | '\"' | '\\r' | '\\n' ) )* '\"'
            {
            match('\"'); 
            // G:\\workspace\\grammar-sandbox\\src\\uk\\co\\colinhowe\\glimpse\\compiler\\ast\\Glimpse.g:45:9: ( EscapeSequence | ~ ( '\\\\' | '\"' | '\\r' | '\\n' ) )*
            loop3:
            do {
                int alt3=3;
                int LA3_0 = input.LA(1);

                if ( (LA3_0=='\\') ) {
                    alt3=1;
                }
                else if ( ((LA3_0>='\u0000' && LA3_0<='\t')||(LA3_0>='\u000B' && LA3_0<='\f')||(LA3_0>='\u000E' && LA3_0<='!')||(LA3_0>='#' && LA3_0<='[')||(LA3_0>=']' && LA3_0<='\uFFFF')) ) {
                    alt3=2;
                }


                switch (alt3) {
            	case 1 :
            	    // G:\\workspace\\grammar-sandbox\\src\\uk\\co\\colinhowe\\glimpse\\compiler\\ast\\Glimpse.g:45:13: EscapeSequence
            	    {
            	    mEscapeSequence(); 

            	    }
            	    break;
            	case 2 :
            	    // G:\\workspace\\grammar-sandbox\\src\\uk\\co\\colinhowe\\glimpse\\compiler\\ast\\Glimpse.g:46:13: ~ ( '\\\\' | '\"' | '\\r' | '\\n' )
            	    {
            	    if ( (input.LA(1)>='\u0000' && input.LA(1)<='\t')||(input.LA(1)>='\u000B' && input.LA(1)<='\f')||(input.LA(1)>='\u000E' && input.LA(1)<='!')||(input.LA(1)>='#' && input.LA(1)<='[')||(input.LA(1)>=']' && input.LA(1)<='\uFFFF') ) {
            	        input.consume();

            	    }
            	    else {
            	        MismatchedSetException mse = new MismatchedSetException(null,input);
            	        recover(mse);
            	        throw mse;}


            	    }
            	    break;

            	default :
            	    break loop3;
                }
            } while (true);

            match('\"'); 

            }

            state.type = _type;
            state.channel = _channel;
        }
        finally {
        }
    }
    // $ANTLR end "STRINGLITERAL"

    // $ANTLR start "EscapeSequence"
    public final void mEscapeSequence() throws RecognitionException {
        try {
            // G:\\workspace\\grammar-sandbox\\src\\uk\\co\\colinhowe\\glimpse\\compiler\\ast\\Glimpse.g:51:5: ( '\\\\' ( 'b' | 't' | 'n' | 'f' | 'r' | '\\\"' | '\\'' | '\\\\' | ( '0' .. '3' ) ( '0' .. '7' ) ( '0' .. '7' ) | ( '0' .. '7' ) ( '0' .. '7' ) | ( '0' .. '7' ) ) )
            // G:\\workspace\\grammar-sandbox\\src\\uk\\co\\colinhowe\\glimpse\\compiler\\ast\\Glimpse.g:51:9: '\\\\' ( 'b' | 't' | 'n' | 'f' | 'r' | '\\\"' | '\\'' | '\\\\' | ( '0' .. '3' ) ( '0' .. '7' ) ( '0' .. '7' ) | ( '0' .. '7' ) ( '0' .. '7' ) | ( '0' .. '7' ) )
            {
            match('\\'); 
            // G:\\workspace\\grammar-sandbox\\src\\uk\\co\\colinhowe\\glimpse\\compiler\\ast\\Glimpse.g:51:14: ( 'b' | 't' | 'n' | 'f' | 'r' | '\\\"' | '\\'' | '\\\\' | ( '0' .. '3' ) ( '0' .. '7' ) ( '0' .. '7' ) | ( '0' .. '7' ) ( '0' .. '7' ) | ( '0' .. '7' ) )
            int alt4=11;
            alt4 = dfa4.predict(input);
            switch (alt4) {
                case 1 :
                    // G:\\workspace\\grammar-sandbox\\src\\uk\\co\\colinhowe\\glimpse\\compiler\\ast\\Glimpse.g:52:18: 'b'
                    {
                    match('b'); 

                    }
                    break;
                case 2 :
                    // G:\\workspace\\grammar-sandbox\\src\\uk\\co\\colinhowe\\glimpse\\compiler\\ast\\Glimpse.g:53:18: 't'
                    {
                    match('t'); 

                    }
                    break;
                case 3 :
                    // G:\\workspace\\grammar-sandbox\\src\\uk\\co\\colinhowe\\glimpse\\compiler\\ast\\Glimpse.g:54:18: 'n'
                    {
                    match('n'); 

                    }
                    break;
                case 4 :
                    // G:\\workspace\\grammar-sandbox\\src\\uk\\co\\colinhowe\\glimpse\\compiler\\ast\\Glimpse.g:55:18: 'f'
                    {
                    match('f'); 

                    }
                    break;
                case 5 :
                    // G:\\workspace\\grammar-sandbox\\src\\uk\\co\\colinhowe\\glimpse\\compiler\\ast\\Glimpse.g:56:18: 'r'
                    {
                    match('r'); 

                    }
                    break;
                case 6 :
                    // G:\\workspace\\grammar-sandbox\\src\\uk\\co\\colinhowe\\glimpse\\compiler\\ast\\Glimpse.g:57:18: '\\\"'
                    {
                    match('\"'); 

                    }
                    break;
                case 7 :
                    // G:\\workspace\\grammar-sandbox\\src\\uk\\co\\colinhowe\\glimpse\\compiler\\ast\\Glimpse.g:58:18: '\\''
                    {
                    match('\''); 

                    }
                    break;
                case 8 :
                    // G:\\workspace\\grammar-sandbox\\src\\uk\\co\\colinhowe\\glimpse\\compiler\\ast\\Glimpse.g:59:18: '\\\\'
                    {
                    match('\\'); 

                    }
                    break;
                case 9 :
                    // G:\\workspace\\grammar-sandbox\\src\\uk\\co\\colinhowe\\glimpse\\compiler\\ast\\Glimpse.g:61:18: ( '0' .. '3' ) ( '0' .. '7' ) ( '0' .. '7' )
                    {
                    // G:\\workspace\\grammar-sandbox\\src\\uk\\co\\colinhowe\\glimpse\\compiler\\ast\\Glimpse.g:61:18: ( '0' .. '3' )
                    // G:\\workspace\\grammar-sandbox\\src\\uk\\co\\colinhowe\\glimpse\\compiler\\ast\\Glimpse.g:61:19: '0' .. '3'
                    {
                    matchRange('0','3'); 

                    }

                    // G:\\workspace\\grammar-sandbox\\src\\uk\\co\\colinhowe\\glimpse\\compiler\\ast\\Glimpse.g:61:29: ( '0' .. '7' )
                    // G:\\workspace\\grammar-sandbox\\src\\uk\\co\\colinhowe\\glimpse\\compiler\\ast\\Glimpse.g:61:30: '0' .. '7'
                    {
                    matchRange('0','7'); 

                    }

                    // G:\\workspace\\grammar-sandbox\\src\\uk\\co\\colinhowe\\glimpse\\compiler\\ast\\Glimpse.g:61:40: ( '0' .. '7' )
                    // G:\\workspace\\grammar-sandbox\\src\\uk\\co\\colinhowe\\glimpse\\compiler\\ast\\Glimpse.g:61:41: '0' .. '7'
                    {
                    matchRange('0','7'); 

                    }


                    }
                    break;
                case 10 :
                    // G:\\workspace\\grammar-sandbox\\src\\uk\\co\\colinhowe\\glimpse\\compiler\\ast\\Glimpse.g:63:18: ( '0' .. '7' ) ( '0' .. '7' )
                    {
                    // G:\\workspace\\grammar-sandbox\\src\\uk\\co\\colinhowe\\glimpse\\compiler\\ast\\Glimpse.g:63:18: ( '0' .. '7' )
                    // G:\\workspace\\grammar-sandbox\\src\\uk\\co\\colinhowe\\glimpse\\compiler\\ast\\Glimpse.g:63:19: '0' .. '7'
                    {
                    matchRange('0','7'); 

                    }

                    // G:\\workspace\\grammar-sandbox\\src\\uk\\co\\colinhowe\\glimpse\\compiler\\ast\\Glimpse.g:63:29: ( '0' .. '7' )
                    // G:\\workspace\\grammar-sandbox\\src\\uk\\co\\colinhowe\\glimpse\\compiler\\ast\\Glimpse.g:63:30: '0' .. '7'
                    {
                    matchRange('0','7'); 

                    }


                    }
                    break;
                case 11 :
                    // G:\\workspace\\grammar-sandbox\\src\\uk\\co\\colinhowe\\glimpse\\compiler\\ast\\Glimpse.g:65:18: ( '0' .. '7' )
                    {
                    // G:\\workspace\\grammar-sandbox\\src\\uk\\co\\colinhowe\\glimpse\\compiler\\ast\\Glimpse.g:65:18: ( '0' .. '7' )
                    // G:\\workspace\\grammar-sandbox\\src\\uk\\co\\colinhowe\\glimpse\\compiler\\ast\\Glimpse.g:65:19: '0' .. '7'
                    {
                    matchRange('0','7'); 

                    }


                    }
                    break;

            }


            }

        }
        finally {
        }
    }
    // $ANTLR end "EscapeSequence"

    public void mTokens() throws RecognitionException {
        // G:\\workspace\\grammar-sandbox\\src\\uk\\co\\colinhowe\\glimpse\\compiler\\ast\\Glimpse.g:1:8: ( NODE | T__10 | ID | WHITESPACE | STRINGLITERAL )
        int alt5=5;
        alt5 = dfa5.predict(input);
        switch (alt5) {
            case 1 :
                // G:\\workspace\\grammar-sandbox\\src\\uk\\co\\colinhowe\\glimpse\\compiler\\ast\\Glimpse.g:1:10: NODE
                {
                mNODE(); 

                }
                break;
            case 2 :
                // G:\\workspace\\grammar-sandbox\\src\\uk\\co\\colinhowe\\glimpse\\compiler\\ast\\Glimpse.g:1:15: T__10
                {
                mT__10(); 

                }
                break;
            case 3 :
                // G:\\workspace\\grammar-sandbox\\src\\uk\\co\\colinhowe\\glimpse\\compiler\\ast\\Glimpse.g:1:21: ID
                {
                mID(); 

                }
                break;
            case 4 :
                // G:\\workspace\\grammar-sandbox\\src\\uk\\co\\colinhowe\\glimpse\\compiler\\ast\\Glimpse.g:1:24: WHITESPACE
                {
                mWHITESPACE(); 

                }
                break;
            case 5 :
                // G:\\workspace\\grammar-sandbox\\src\\uk\\co\\colinhowe\\glimpse\\compiler\\ast\\Glimpse.g:1:35: STRINGLITERAL
                {
                mSTRINGLITERAL(); 

                }
                break;

        }

    }


    protected DFA4 dfa4 = new DFA4(this);
    protected DFA5 dfa5 = new DFA5(this);
    static final String DFA4_eotS =
        "\11\uffff\2\13\1\uffff\1\15\2\uffff";
    static final String DFA4_eofS =
        "\17\uffff";
    static final String DFA4_minS =
        "\1\42\10\uffff\2\60\1\uffff\1\60\2\uffff";
    static final String DFA4_maxS =
        "\1\164\10\uffff\2\67\1\uffff\1\67\2\uffff";
    static final String DFA4_acceptS =
        "\1\uffff\1\1\1\2\1\3\1\4\1\5\1\6\1\7\1\10\2\uffff\1\13\1\uffff"+
        "\1\12\1\11";
    static final String DFA4_specialS =
        "\17\uffff}>";
    static final String[] DFA4_transitionS = {
            "\1\6\4\uffff\1\7\10\uffff\4\11\4\12\44\uffff\1\10\5\uffff\1"+
            "\1\3\uffff\1\4\7\uffff\1\3\3\uffff\1\5\1\uffff\1\2",
            "",
            "",
            "",
            "",
            "",
            "",
            "",
            "",
            "\10\14",
            "\10\15",
            "",
            "\10\16",
            "",
            ""
    };

    static final short[] DFA4_eot = DFA.unpackEncodedString(DFA4_eotS);
    static final short[] DFA4_eof = DFA.unpackEncodedString(DFA4_eofS);
    static final char[] DFA4_min = DFA.unpackEncodedStringToUnsignedChars(DFA4_minS);
    static final char[] DFA4_max = DFA.unpackEncodedStringToUnsignedChars(DFA4_maxS);
    static final short[] DFA4_accept = DFA.unpackEncodedString(DFA4_acceptS);
    static final short[] DFA4_special = DFA.unpackEncodedString(DFA4_specialS);
    static final short[][] DFA4_transition;

    static {
        int numStates = DFA4_transitionS.length;
        DFA4_transition = new short[numStates][];
        for (int i=0; i<numStates; i++) {
            DFA4_transition[i] = DFA.unpackEncodedString(DFA4_transitionS[i]);
        }
    }

    class DFA4 extends DFA {

        public DFA4(BaseRecognizer recognizer) {
            this.recognizer = recognizer;
            this.decisionNumber = 4;
            this.eot = DFA4_eot;
            this.eof = DFA4_eof;
            this.min = DFA4_min;
            this.max = DFA4_max;
            this.accept = DFA4_accept;
            this.special = DFA4_special;
            this.transition = DFA4_transition;
        }
        public String getDescription() {
            return "51:14: ( 'b' | 't' | 'n' | 'f' | 'r' | '\\\"' | '\\'' | '\\\\' | ( '0' .. '3' ) ( '0' .. '7' ) ( '0' .. '7' ) | ( '0' .. '7' ) ( '0' .. '7' ) | ( '0' .. '7' ) )";
        }
    }
    static final String DFA5_eotS =
        "\1\uffff\1\3\4\uffff\2\3\1\11\1\uffff";
    static final String DFA5_eofS =
        "\12\uffff";
    static final String DFA5_minS =
        "\1\11\1\157\4\uffff\1\144\1\145\1\101\1\uffff";
    static final String DFA5_maxS =
        "\1\172\1\157\4\uffff\1\144\1\145\1\172\1\uffff";
    static final String DFA5_acceptS =
        "\2\uffff\1\2\1\3\1\4\1\5\3\uffff\1\1";
    static final String DFA5_specialS =
        "\12\uffff}>";
    static final String[] DFA5_transitionS = {
            "\2\4\1\uffff\2\4\22\uffff\1\4\1\uffff\1\5\27\uffff\1\2\6\uffff"+
            "\32\3\6\uffff\15\3\1\1\14\3",
            "\1\6",
            "",
            "",
            "",
            "",
            "\1\7",
            "\1\10",
            "\32\3\6\uffff\32\3",
            ""
    };

    static final short[] DFA5_eot = DFA.unpackEncodedString(DFA5_eotS);
    static final short[] DFA5_eof = DFA.unpackEncodedString(DFA5_eofS);
    static final char[] DFA5_min = DFA.unpackEncodedStringToUnsignedChars(DFA5_minS);
    static final char[] DFA5_max = DFA.unpackEncodedStringToUnsignedChars(DFA5_maxS);
    static final short[] DFA5_accept = DFA.unpackEncodedString(DFA5_acceptS);
    static final short[] DFA5_special = DFA.unpackEncodedString(DFA5_specialS);
    static final short[][] DFA5_transition;

    static {
        int numStates = DFA5_transitionS.length;
        DFA5_transition = new short[numStates][];
        for (int i=0; i<numStates; i++) {
            DFA5_transition[i] = DFA.unpackEncodedString(DFA5_transitionS[i]);
        }
    }

    class DFA5 extends DFA {

        public DFA5(BaseRecognizer recognizer) {
            this.recognizer = recognizer;
            this.decisionNumber = 5;
            this.eot = DFA5_eot;
            this.eof = DFA5_eof;
            this.min = DFA5_min;
            this.max = DFA5_max;
            this.accept = DFA5_accept;
            this.special = DFA5_special;
            this.transition = DFA5_transition;
        }
        public String getDescription() {
            return "1:1: Tokens : ( NODE | T__10 | ID | WHITESPACE | STRINGLITERAL );";
        }
    }
 

}