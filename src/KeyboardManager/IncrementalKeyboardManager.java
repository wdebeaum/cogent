/*
 * KeyboardManager.java
 *
 * Dave Costello, costello@cs.rochester.edu, 19 Oct 1998
 * $Id: IncrementalKeyboardManager.java,v 1.1 2005/10/27 11:36:58 stoness Exp $
 */

package TRIPS.KeyboardManager;

import java.io.File;
import java.io.FilenameFilter;

import java.io.IOException;
import java.awt.BorderLayout;
import java.awt.Container;
import java.awt.Font;
import java.awt.event.ActionListener;
import java.awt.event.KeyListener;
import java.awt.event.ActionEvent;
import java.awt.event.KeyEvent;
import javax.swing.JComboBox;
import javax.swing.JFrame;
import javax.swing.JLabel;
import javax.swing.JMenu;
import javax.swing.JMenuBar;
import javax.swing.JMenuItem;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JTextArea;
import TRIPS.TripsModule.StandardTripsModule;
import TRIPS.KQML.KQMLPerformative;
import TRIPS.KQML.KQMLList;
import TRIPS.KQML.KQMLString;
import TRIPS.util.GeometrySpec;
import TRIPS.util.StringUtils;

public class IncrementalKeyboardManager extends StandardTripsModule
				implements ActionListener, KeyListener {
    //
    // Fields
    //
    private String title = "Incremental Keyboard Manager";
    private boolean iconic = false;
    private GeometrySpec geometry;
    private static String DEFAULT_GEOMETRY = "80x5+0-0";
    private int columns = 80;
    private int rows = 5;
    private int fontsize = 14;
    private boolean showMenuBar = true;
    private JFrame frame;
    private JTextArea textArea;
    private int lastCaret;
    private static String NEWLINE = System.getProperty("line.separator");
    JComboBox channelComboBox;
	JComboBox incrementalComboBox;
    String[] channelLabels = { "Phone", "Keyboard" };
    String channelDefault = "Keyboard";
	String[] incrementalLabels = {"Word-by-Word", "Sentence-end Words", "Full Sentence"};
	String incrementalDefault = "Full Sentence";

	final static int INCREMENTAL = 0;
	final static int SENTENCEENDWORDS = 1;
	final static int NONINCREMENTAL = 2;

	String[] scoreFunctionLabels = {"RF-noop","RF-linear-w-boost"};
	String scoreFunctionDefault = "RF-noop";


	private int wordCount = 0;
    //
    // Constructor
    //
    public IncrementalKeyboardManager(String argv[], boolean isApplication) {
	super(argv, isApplication);
    }
    public IncrementalKeyboardManager(String argv[]) {
	this(argv, false);
    }
    //
    // StandardTripsModule method
    //
    public void init() {
	// Unless overriden by parameters...
	name = "keyboard";
	// Perform standard initializations
	super.init();
	// Then parse our parameters
	handleParameters();
	// Create the widgets
	createWidgets();
	// Print the initial prompt
	userPrompt(0);
	// We need to subscribe to speech-in to get spoken user input
	try {
	    KQMLPerformative perf =
		KQMLPerformative.fromString("(subscribe :content (tell &key :content (utterance . *)))");
	    send(perf);
	    perf = KQMLPerformative.fromString("(subscribe :content (tell &key :content (spoken . *)))");
	    send(perf);
	} catch (IOException ex) {
	    error("Yow! Subscription failed: " + ex);
	}
	// Tell the Facilitator we are ready
	ready();
    }
    // Parse parameters
    private void handleParameters() {
	String value;
	if ((value = getParameter("-title")) != null) {
	    title = value;
	}
	if ((value = getParameter("-iconic")) != null) {
	    iconic = StringUtils.stringToBoolean(value);
	}
	if ((value = getParameter("-geometry")) != null) {
	    geometry = new GeometrySpec(value);
	} else {
	    geometry = new GeometrySpec(DEFAULT_GEOMETRY);
	}	    
	columns = geometry.width;
	rows = geometry.height;
	if ((value = getParameter("-columns")) != null) {
	    columns = Integer.parseInt(value);
	}
	if ((value = getParameter("-rows")) != null) {
	    rows = Integer.parseInt(value);
	}
	if ((value = getParameter("-fontsize")) != null) {
	    fontsize = Integer.parseInt(value);
	}
	if ((value = getParameter("-showMenuBar")) != null) {
	    showMenuBar = StringUtils.stringToBoolean(value);
	}
	if ((value = getParameter("-channel")) != null) {
	    channelDefault = value;
	}
    }
    // Create the widgets for the applet
    private void createWidgets() {
	// Create toplevel frame
	frame = new JFrame();
	frame.setTitle(title);
	frame.setState(iconic ? JFrame.ICONIFIED : JFrame.NORMAL);
	Container contents = frame.getContentPane();
	contents.setLayout(new BorderLayout());
	// Info boxes:
	JPanel panel = new JPanel();
	JLabel label;
	label = new JLabel("Incrementality:");
	panel.add(label);
	channelComboBox = new JComboBox(channelLabels);
	channelComboBox.setEditable(true);
	channelComboBox.setSelectedItem(channelDefault);
//	panel.add(channelComboBox);
	
	incrementalComboBox = new JComboBox(incrementalLabels);
	incrementalComboBox.setEditable(false);
	incrementalComboBox.setSelectedItem(incrementalDefault);
	panel.add(incrementalComboBox);


// 	// SET UP KB COMBO BOX
// 	String[] kbFilenames = null;

// 	try
// 		{
// 			File currentDir = new File("..");  // should be src
// 			//System.out.println(currentDir.getCanonicalPath());
// 			File kbDir = new File(currentDir.getCanonicalPath() 
// 								  + File.separator +"Simulator");
// 			//	System.out.println(kbDir.getCanonicalPath());
// 			FilenameFilter filter = new FilenameFilter() {
// 					public boolean accept(File f, String fn)
// 					{
// 						if (fn.endsWith(".kb")) return true;
// 						return false;
// 					}
// 				};
			
// 			kbFilenames = kbDir.list(filter);

// 			}
// 		catch (Exception e)
// 			{
// 			}


// 	final JComboBox kbComboBox = new JComboBox(kbFilenames);
// 	kbComboBox.addItem("Set KB State");
// 	kbComboBox.setSelectedItem("Set KB State");
// 	kbComboBox.setEditable(false);
// 	kbComboBox.addActionListener (new ActionListener(){
// 			public void actionPerformed(ActionEvent e)
// 			{
// 				String s = (String) kbComboBox.getSelectedItem();
// 				if (!s.equals("Set KB State")) sendKBMessage(s);
// 			}
// 		}
// 								  );

// 	panel.add(kbComboBox);

// 	final JComboBox sfComboBox = new JComboBox(scoreFunctionLabels);
// 	sfComboBox.setSelectedItem(scoreFunctionDefault);
// 	sfComboBox.setEditable(false);
// 	sfComboBox.addActionListener ( new ActionListener(){
// 			public void actionPerformed (ActionEvent e)
// 			{
// 				String s = (String) sfComboBox.getSelectedItem();
// 				sendIPMessage(s);
// 			}
// 		});

// 	panel.add(sfComboBox);

	contents.add(panel, BorderLayout.NORTH);
        // TextArea (note width and height in rows/cols not pixels)
        textArea = new JTextArea(rows, columns);
        textArea.setFont(new Font("Monospaced", Font.BOLD, fontsize));
        textArea.setEditable(true);
	textArea.requestFocus();
	textArea.setLineWrap(true);
	textArea.setWrapStyleWord(true);
        // Register text area listeners for event callbacks
	textArea.addKeyListener(this);
	// Put text area inside scrollpane
	JScrollPane scroller = new JScrollPane(textArea);
	// Add scrollpane to frame
        contents.add(scroller, BorderLayout.CENTER); 
	// Possibly add menubar
        if (showMenuBar) {
	    createMenus();
	}
	// Pack the frame
	frame.pack();
	// Adjust location after packing has set size
	geometry.setLocationOfFrame(frame);
	// Show the frame
	frame.setVisible(true);
    }
    // Create the menus for the applet
    private void createMenus() {
        // Create the menu bar
	JMenuBar menubar = new JMenuBar();
	JMenu menu;
	JMenuItem item;
        // Control menu 
	menu = new JMenu("Control");
	item = new JMenuItem("Quit", KeyEvent.VK_Q); 
	item.setActionCommand("Quit");
	item.addActionListener(this);
	menu.add(item);
	menubar.add(menu);
        // Font menu
	menu = new JMenu("Font");
	String fontnames[] = { "6", "8", "12", "14", "24", "36", "48" };
	int i;
	for (i=0; i < fontnames.length; i++) {
	    item = new JMenuItem(fontnames[i]);
	    item.addActionListener(this);
	    menu.add(item);
	}
	menubar.add(menu);
        // Help menu
	menu = new JMenu("Help");
	item = new JMenuItem("No help available");
	menu.add(item);
	// Not in JDK1.2...
	//menubar.setHelpMenu(menu);
	menubar.add(menu);
	// Add the menubar to the frame
	frame.setJMenuBar(menubar);
    }
    // Override StandardTripsModule method to cleanup GUI
    public void exit(int n) {
	if (frame != null) {
	    frame.dispose();
	}
	super.exit(n);
    }
    //
    // KeyListener methods
    //
    // TODO: Cleanup so we send WORD and BACKTO for spaces
    //
    boolean startedSpeaking = false;
    int uttnum = 0;
	//boolean isCommandLine = false;

    public void keyTyped(KeyEvent e) {}
    public void keyPressed(KeyEvent e) {}
    public void keyReleased(KeyEvent e) {
	int keycode = e.getKeyCode();
	String channel = (String)channelComboBox.getSelectedItem();
	String incremental = (String) incrementalComboBox.getSelectedItem();
	// NC - If they typed a letter, then they started an utterance
	//    - Navigating with arrows shouldn't send a start conversation
	//    - Assumes all utterances start with a letter...
	if( keycode >= KeyEvent.VK_A && keycode <= KeyEvent.VK_Z ) {
	    // Is this the first thing they typed this utt?
	    if (!startedSpeaking) {
			wordCount = 0;
			// Yes: send STARTED-SPEAKING message
			uttnum += 1;
			KQMLPerformative perf = new KQMLPerformative("tell");
			KQMLList content = new KQMLList();
			content.add("started-speaking");
			content.add(":channel");
			content.add(channel);
			content.add(":direction");
			content.add("input");
			content.add(":mode");
			content.add("text");
			content.add(":uttnum");
			content.add(Integer.toString(uttnum));
			perf.setParameter(":content", content);
			send(perf);
			// Don't do it again this utt
			startedSpeaking = true;
	    }
	}

	// Interprets things as a command if they start with a colon
	//if (!startedSpeaking && keycode == KeyEvent.VK_COLON) {
	//	isCommandLine = true;
	//}
		

	// Was this an end-of-word? (e.g. space or END_OF_LINE)
	if (incremental.equals(incrementalLabels[INCREMENTAL]) &&
		(keycode == KeyEvent.VK_SPACE || keycode == KeyEvent.VK_ENTER)) {
	    int caret = textArea.getCaretPosition();
		String text = textArea.getText().substring(0,textArea.getText().length()-1);
		String word = text.substring(text.lastIndexOf(" ")+1);
	    // Select region from last prompt (not including newline)
	    //textArea.select(lastCaret, caret-1);
        //String text = textArea.getSelectedText();

		
		// Huge UGLY kludge to allow specifying the kb to use from the keyboard manager.

		//if (word.startsWith(":KB-"))
		//	{
		//		sendKBMessage(word.substring(4));
		//		return;
		//	}

		wordCount += 1;

	    // Send KQML messages

		// Find out if there are word alternatives here;  send a WORD
		// message with the same index for each.
		
		String[] words = word.split("/");
		for (int i = 0; i < words.length; i++)
			{
				sendWordMessage(words[i],channel,wordCount,uttnum);
			}
	    //KQMLPerformative perf = new KQMLPerformative("tell");
	    // First STOPPED-SPEAKING
	    //KQMLList content = new KQMLList();
		//content.add("word");
	    //content.add(new KQMLString(word));
	    //content.add(":index");
	    //content.add(Integer.toString(wordCount));
	    //content.add(":uttnum");
	    //content.add(Integer.toString(uttnum));
	    //content.add(":channel");
	    //content.add(channel);
	    //content.add(":direction");
	    //content.add("input");
	    //perf.setParameter(":content", content);
	    //send(perf);
	}

	// Was this an end-of-line?
	if (keycode == KeyEvent.VK_ENTER) {
	    // Yes: send STOPPED-SPEAKING and UTTERANCE messages (and WORD)
		int caret = textArea.getCaretPosition();
	    // Select region from last prompt (not including newline)
	    textArea.select(lastCaret, caret-1);
		String text = textArea.getSelectedText();
		// Send KQML messages
		KQMLPerformative perf = new KQMLPerformative("tell");
		KQMLList content = new KQMLList();
		
		// Then a WORD message with all the words, if full
		// non-incremental mode is on.
		if (incremental.equals(incrementalLabels[NONINCREMENTAL]))
		{
			content = new KQMLList();
			content.add("word");
			content.add(new KQMLString(text));
			content.add(":index");
			content.add("1");
			content.add(":uttnum");
			content.add(Integer.toString(uttnum));
			content.add(":channel");
			content.add(channel);
			content.add(":direction");
			content.add("input");
			if (text.indexOf("/") != -1)
				{
					content.add(":alternatives");
					content.add("true");
				}
			perf.setParameter(":content", content);
		    send(perf);	
		}
		// Otherwise, a WORD message for each word.
		if (incremental.equals(incrementalLabels[SENTENCEENDWORDS]))
		{
			String[] words = text.split(" ");
			
			for (int i = 0; i < words.length; i++)
				{
					String[] alternatives = words[i].split("/");
					for (int j = 0; j < alternatives.length; j++)
						{
							
							sendWordMessage(alternatives[j],channel,i+1,uttnum);
						}
				}

			//content = new KQMLList();
			//content.add("word");
			//content.add(new KQMLString(text));
			//content.add(":index");
			//content.add("1");
			//content.add(":uttnum");
			//content.add(Integer.toString(uttnum));
			//content.add(":channel");
			//content.add(channel);
			//content.add(":direction");
			//content.add("input");
			//perf.setParameter(":content", content);
		    //send(perf);
		}

		// Then Stopped-speaking
		content = new KQMLList();
		content.add("stopped-speaking");
		content.add(":channel");
		content.add(channel);
		content.add(":direction");
		content.add("input");
		content.add(":mode");
		content.add("text");
		content.add(":uttnum");
		content.add(Integer.toString(uttnum));
		perf.setParameter(":content", content);
		send(perf);

		// Finally an UTTERANCE message (includes the text for transcriptors)
		content = new KQMLList();
		content.add("utterance");
		content.add(":channel");
		content.add(channel);
		content.add(":direction");
		content.add("input");
		content.add(":mode");
		content.add("text");
		content.add(":uttnum");
		content.add(Integer.toString(uttnum));
		content.add(":text");
		content.add(new KQMLString(text));
		perf.setParameter(":content", content);
		send(perf);
		
		// Reset
	    textArea.select(caret, caret);
	    userPrompt(caret);
	    startedSpeaking = false;
	}
    }

	protected void sendKBMessage(String filename)
	{
		KQMLPerformative perf = new KQMLPerformative("REQUEST");
	    // First STOPPED-SPEAKING
	    KQMLList content = new KQMLList();
		content.add("KB-REF-CHECK");
		content.add(":SET-KB");
		content.add(new KQMLString(filename));
		perf.setParameter(":CONTENT",content);
		perf.setParameter(":REPLY-WITH","SET-KB");
		send(perf);
	}

	protected void sendIPMessage(String scoreFunction)
	{
		String commandString = 
			"(setf parser::*reference-modify-entry-function* 'parser::" 
			+ scoreFunction + ")";

		KQMLPerformative perf = new KQMLPerformative("REQUEST");
	    // First STOPPED-SPEAKING
	    KQMLList content = new KQMLList();
		content.add("EVAL");
		content.add(commandString);
		perf.setParameter(":CONTENT",content);
		perf.setParameter(":RECEIVER", "Parser");
		send(perf);
	}

	protected void sendWordMessage(String word, String channel, int wordCount, int uttnum)
	{
		KQMLPerformative perf = new KQMLPerformative("tell");
	    // First STOPPED-SPEAKING
	    KQMLList content = new KQMLList();
		content.add("word");
	    content.add(new KQMLString(word));
	    content.add(":frame");
	    content.add("(" + Integer.toString(wordCount) + ")");
		content.add(":uttnum");
	    content.add(Integer.toString(uttnum));
	    content.add(":channel");
	    content.add(channel);
	    content.add(":direction");
	    content.add("input");
	    perf.setParameter(":content", content);
	    send(perf);

		
	}



    //
    // ActionListener methods
    //
    public void actionPerformed(ActionEvent evt) {
        String cmd = evt.getActionCommand();  
        if (cmd.equals("Quit")) {
	    exit(0);
	} else if (cmd.equals("48")) {
	    textArea.setFont(new Font("Monospaced", Font.BOLD, 48));
	} else if (cmd.equals("36")) {
	    textArea.setFont(new Font("Monospaced", Font.BOLD, 36));
	} else if (cmd.equals("24")) {
	    textArea.setFont(new Font("Monospaced", Font.BOLD, 24));
	} else if (cmd.equals("14")) {
	    textArea.setFont(new Font("Monospaced", Font.BOLD, 14));
	} else if (cmd.equals("12")) {
	    textArea.setFont(new Font("Monospaced", Font.BOLD, 12));
	} else if (cmd.equals("8")) {
	    textArea.setFont(new Font("Monospaced", Font.BOLD, 8));
	} else if (cmd.equals("6")) {
	    textArea.setFont(new Font("Monospaced", Font.BOLD, 6));
	}
    }
    //
    // Text window methods
    //
    // Paste a system message to text area preceded by "sys>" tag
    private void insertSysMsg(String line) {
	// Insert this text ahead of the user's current line
	textArea.insert("SYS> " + line + NEWLINE, lastCaret - 5);
	// Update new start of user's text
	lastCaret += line.length() + 6;
	// Update position of caret
	textArea.setCaretPosition(lastCaret);
    }
    // Ditto for user input (from speech recognition)
    private void insertUsrMsg(String line) {
	// Insert this text ahead of the user's current line
	textArea.insert("USR> " + line + NEWLINE, lastCaret - 5);
	// Update new start of user's text
	lastCaret += line.length() + 6;
	// Update position of caret
	textArea.setCaretPosition(lastCaret);
    }
    private void userPrompt(int caret) {
	textArea.insert("USR> ", caret);
	lastCaret = caret + 5;
	// Update position of caret
	textArea.setCaretPosition(lastCaret);
    }
    //
    // KQMLReceiver methods overriding StandardTripsModule defaults
    //
    public void receiveEOF() {
	exit(0);
    }
    public void receiveTell(KQMLPerformative msg, Object contentobj) {
	debug("receiveTell: msg=" + msg);
	if (!(contentobj instanceof KQMLList)) {
	    errorReply(msg, "content was not a list");
	    return;
	}
	KQMLList content = (KQMLList)contentobj;
	String content0 = content.nth(0).toString();
	if (content0.equalsIgnoreCase("utterance")) {
	    debug("receiveTell: content is an UTTERANCE");
	    String mode = content.getKeywordArg(":mode").toString();
	    KQMLString text = (KQMLString)content.getKeywordArg(":text");
	    debug("receiveTell: mode=" + mode + ", text=\"" + text + "\"");
	    if (mode == null) {
		errorReply(msg, "missing :mode in UTTERANCE");
	    } else if (text == null) {
		errorReply(msg, "missing :text in UTTERANCE");
	    } else {
		if (mode.equalsIgnoreCase("speech")) {
		    insertUsrMsg(text.stringValue());
		} else {
		    // In fact, we don't do this because it's from us!
		    // insertUsrMsg(text.stringValue());
		}
	    }
	} else if (content0.equalsIgnoreCase("spoken")) {
	    KQMLString text = (KQMLString)content.getKeywordArg(":what");
	    insertSysMsg(text.stringValue());
	} else if (content0.equalsIgnoreCase("start-conversation")) {
	    reset();
	    showWindow();
	} else if (content0.equalsIgnoreCase("end-conversation")) {
	    hideWindow();
	} else if (content0.equalsIgnoreCase("new-scenario") ||
		   content0.equalsIgnoreCase("modify-scenario")) {
	    // Ignore
	} else {
	    errorReply(msg, "bad tell: " + content0);
	}
    }
    public void receiveRequest(KQMLPerformative msg, Object contentobj) {
	if (!(contentobj instanceof KQMLList)) {
	    errorReply(msg, "content was not a list");
	    return;
	}
	KQMLList content = (KQMLList)contentobj;
	String content0 = content.nth(0).toString();
	if (content0.equalsIgnoreCase("exit")) {
	    int status = 0;
	    if (content.length() > 1) {
		status = Integer.parseInt(content.nth(1).toString());
	    }
 	    exit(status);
	} else if (content0.equalsIgnoreCase("hide-window")) {
	    hideWindow();
	} else if (content0.equalsIgnoreCase("show-window")) {
	    showWindow();
	} else if (content0.equalsIgnoreCase("chdir")) {
	    // Ignore
	} else if (content0.equalsIgnoreCase("reset")) {
	    reset();
	} else if (content0.equalsIgnoreCase("grab")) {
	    System.err.println("tkeyboard: grab request not implemented");
	} else if (content0.equalsIgnoreCase("ungrab")) {
	    System.err.println("tkeyboard: ungrab request not implemented");
	} else if (content0.equalsIgnoreCase("add-text")) {
	    System.err.println("tkeyboard: add-text request not implemented");
	} else if (content0.equalsIgnoreCase("say")) {
	    // SAY messages come for speech-out
	    KQMLString kstr = (KQMLString)content.nth(1);
	    String str = kstr.stringValue();
	    // Remove truetalk escapes
	    int i, j;
	    while ((i = str.indexOf("\\")) != -1) {
		if ((j = str.indexOf(" ", i)) == -1) {
		    j = str.length();
		} else {
		    j += 1;
		}
		str = str.substring(0, i) + str.substring(j);
	    }
	    insertSysMsg(str);
	} else {
	    errorReply(msg, "bad request: " + content0);
	}
    }
    // Make window visible
    public void showWindow(){
	frame.setState(JFrame.NORMAL);
    }
    // Make window invisible
    public void hideWindow(){
	frame.setState(JFrame.ICONIFIED);
    }
    // Erase contents of window and reset everything
    public void reset(){
	textArea.setText("");
	userPrompt(0);
    }
    //
    // When run as an application
    //
    public static void main(String argv[]) {
		new IncrementalKeyboardManager(argv, true).run();
    }
}
