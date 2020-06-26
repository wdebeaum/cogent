/*
 * KeyboardManager.java
 *
 * Dave Costello, costello@cs.rochester.edu, 19 Oct 1998
 * $Id: KeyboardManager.java,v 1.25 2020/03/06 19:31:02 wdebeaum Exp $
 */

package TRIPS.KeyboardManager;

import java.io.IOException;
import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.Container;
import java.awt.Font;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.ItemEvent;
import java.awt.event.ItemListener;
import java.awt.event.KeyEvent;
import java.awt.event.KeyListener;
import javax.swing.BorderFactory;
import javax.swing.JCheckBoxMenuItem;
import javax.swing.JComboBox;
import javax.swing.JFrame;
import javax.swing.JLabel;
import javax.swing.JMenu;
import javax.swing.JMenuBar;
import javax.swing.JMenuItem;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JTextArea;
import javax.swing.JScrollBar;
import javax.swing.SwingUtilities;
import java.net.URL;
import javax.sound.sampled.AudioFormat;
import javax.sound.sampled.AudioInputStream;
import javax.sound.sampled.AudioSystem;
import javax.sound.sampled.Clip;
import javax.sound.sampled.DataLine;
import java.util.ArrayList;
import java.util.List;

import TRIPS.KQML.KQMLList;
import TRIPS.KQML.KQMLObject;
import TRIPS.KQML.KQMLPerformative;
import TRIPS.KQML.KQMLString;
import TRIPS.TripsModule.StandardTripsModule;
import TRIPS.util.GeometrySpec;
import TRIPS.util.StringUtils;

/**
 * KeyboardManager (aka., TRIPS Chat) handles keyboard input from the user
 * and displays user and system utterances in a transcript.
 */
public class KeyboardManager extends StandardTripsModule
		    implements ActionListener, KeyListener, ItemListener {
    //
    // Fields
    //
    protected String title = "Keyboard Manager";
    protected boolean iconic = false;
    protected GeometrySpec geometry;
    protected static String DEFAULT_GEOMETRY = "80x5+0-0";
    protected int columns = 80;
    protected int transcriptRows = 4;
    protected int compositionRows = 1;
    protected int fontsize = 14;
    protected boolean showMenuBar = true;
    protected JFrame frame;
    protected JTextArea transcriptArea;
    protected JTextArea compositionArea;
    protected JScrollBar transcriptScrollBar;
    protected int lastCaret = 0;
    protected static String NEWLINE = System.getProperty("line.separator");
    protected JCheckBoxMenuItem beepCheckBox;
    protected boolean showChannels = false;
    protected JComboBox channelComboBox;
    protected String[] channelLabels = { "Phone", "Desktop" };
    protected String channelDefault = "Desktop";
    protected boolean doBeep = true;
    protected boolean dontPrintSilenceTokens = false; // whether to display <SIL> tags
    // for running without a generation module
    protected boolean showGenerateMessages = false;
    protected List<String> utteranceHistory = new ArrayList<String>();
    protected int utteranceHistoryPos = 0; // when navigating the history, this is where we are
    protected final String USR = "USR: ";
    protected final String SYS = "SYS: ";

    /**
     * Construct and return a new KeyboardManager with the given
     * StandardTripsModule parameters.
     */
    public KeyboardManager(String argv[], boolean isApplication) {
	super(argv, isApplication);
    }
    /**
     * Construct and return a new KeyboardManager with the given
     * StandardTripsModule parameters (default is not be a standalone
     * application).
     */
    public KeyboardManager(String argv[]) {
	this(argv, false);
    }

    /**
     * StandardTripsModule initialization method.
     */
    public void init() {
	// Unless overriden by parameters...
	name = "keyboard";
	// Perform standard initializations
	super.init();
	// Then parse our parameters
	handleParameters();
	// Create the widgets
	createWidgets();
	// We need to subscribe to speech-in to get spoken user input
	try {
	    KQMLPerformative perf =
		KQMLPerformative.fromString("(subscribe :content (tell &key :content (utterance . *)))");
	    send(perf);
	    perf = KQMLPerformative.fromString("(subscribe :content (tell &key :content (spoken . *)))");
	    send(perf);
	    // LG 11/16/2015 for CwC:BOB demo -- TODO: take out once we have a GM!
	    if (showGenerateMessages) {
		perf = KQMLPerformative.fromString("(subscribe :content (request &key :content (generate . *)))");
		send(perf);
	    }
	} catch (IOException ex) {
	    error("Yow! Subscription failed: " + ex);
	}
	// Prepare to beep
	if (doBeep) {
	    initBeep();
	}
	// Tell the Facilitator we are ready
	ready();
    }
    /**
     * Parse parameters passed to this component.
     */
    protected void handleParameters() {
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
	transcriptRows = geometry.height - compositionRows;
	if ((value = getParameter("-columns")) != null) {
	    columns = Integer.parseInt(value);
	}
	if ((value = getParameter("-rows")) != null) {
	    transcriptRows = Integer.parseInt(value) - compositionRows;
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
	if ((value = getParameter("-channels")) != null) {
	    showChannels = StringUtils.stringToBoolean(value);
	}
	if ((value = getParameter("-beep")) != null) {
	    doBeep = StringUtils.stringToBoolean(value);
	}
	if ((value = getParameter("-nosilence")) != null) { // whether to pull out <SIL> tags
	    dontPrintSilenceTokens = true;
	}
	if ((value = getParameter("-showGenerate")) != null) {
	    showGenerateMessages = StringUtils.stringToBoolean(value);
	}
    }
    /**
     * Create widgets for this component.
     */
    protected void createWidgets() {
	// Create toplevel frame
	frame = new JFrame();
	frame.setTitle(title);
	frame.setState(iconic ? JFrame.ICONIFIED : JFrame.NORMAL);
	Container contents = frame.getContentPane();
	contents.setLayout(new BorderLayout());
	// Info box(es)
	if (showChannels) {
	    JPanel panel = new JPanel();
	    JLabel label;
	    label = new JLabel("Channel:");
	    panel.add(label);
	    channelComboBox = new JComboBox(channelLabels);
	    channelComboBox.setEditable(true);
	    channelComboBox.setSelectedItem(channelDefault);
	    panel.add(channelComboBox);
	    contents.add(panel, BorderLayout.NORTH);
	}
        // Transcript area (note width and height in rows/cols not pixels)
	JPanel panel = new JPanel(new BorderLayout());
	panel.setBorder(BorderFactory.createEmptyBorder(5,5,5,5));
        transcriptArea = new JTextArea(transcriptRows, columns);
        transcriptArea.setFont(new Font("Monospaced", Font.BOLD, fontsize));
        transcriptArea.setEditable(false);
	transcriptArea.setLineWrap(true);
	transcriptArea.setWrapStyleWord(true);
	transcriptArea.setWrapStyleWord(true);
	transcriptArea.setRequestFocusEnabled(false);
	JScrollPane transcriptScroller = new JScrollPane(transcriptArea);
	transcriptScroller.setVerticalScrollBarPolicy(JScrollPane.VERTICAL_SCROLLBAR_ALWAYS);
	transcriptScrollBar = transcriptScroller.getVerticalScrollBar();
        panel.add(transcriptScroller, BorderLayout.CENTER); 
        contents.add(panel, BorderLayout.CENTER); 
	// Composition area
	panel = new JPanel(new BorderLayout());
	panel.setBorder(BorderFactory.createEmptyBorder(5,5,5,5));
	JLabel label = new JLabel("Chat: ");
	panel.add(label, BorderLayout.WEST);
        compositionArea = new JTextArea(compositionRows, columns);
	compositionArea.setBackground(new Color(175, 230, 255));
        compositionArea.setFont(new Font("Monospaced", Font.BOLD, fontsize));
        compositionArea.setEditable(true);
	compositionArea.setLineWrap(true);
	compositionArea.setWrapStyleWord(true);
	compositionArea.requestFocusInWindow();
	compositionArea.addKeyListener(this);
	JScrollPane compositionScroller = new JScrollPane(compositionArea);
        panel.add(compositionScroller, BorderLayout.CENTER); 
        contents.add(panel, BorderLayout.SOUTH); 
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
    /**
     * Create menubar and menus for this component.
     */
    protected void createMenus() {
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
        // View menu
	menu = new JMenu("View");
	// Font submenu
	JMenu submenu = new JMenu("Font");
	String fontnames[] = { "6", "8", "12", "14", "24", "36", "48" };
	int i;
	for (i=0; i < fontnames.length; i++) {
	    item = new JMenuItem(fontnames[i]);
	    item.addActionListener(this);
	    submenu.add(item);
	}
	menu.add(submenu);
	menubar.add(menu);
        // Sound menu
	menu = new JMenu("Sound");
	beepCheckBox = new JCheckBoxMenuItem("Beep on system utterances", doBeep);
	beepCheckBox.addItemListener(this);
	menu.add(beepCheckBox);
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
    /**
     * Override StandardTripsModule method to cleanup GUI.
     */
    public void exit(int n) {
	if (frame != null) {
	    frame.dispose();
	}
	super.exit(n);
    }

    //
    // KeyListener methods
    //
    //

    boolean startedSpeaking = false;
    int uttnum = 0;

    /**
     * KeyListener method: ignore (work done in keyReleased).
     */
    public void keyPressed(KeyEvent e) {}
    /**
     * KeyListener method: Handle typed user input.
     * TODO: Cleanup so we send WORD and BACKTO for spaces
     */
    public void keyReleased(KeyEvent e) {
	int keycode = e.getKeyCode();
	//System.err.println("keyTyped: e=" + e + ", keycode=" + keycode);
	if (keycode == KeyEvent.VK_ENTER) {
	    handleEndOfLine();
	} else if (keycode == KeyEvent.VK_DOWN || keycode == KeyEvent.VK_UP) {
	    handleHistory(keycode);
	} else {
	    // Other key
	    maybeStartedSpeaking();
	}
    }
    /**
     * KeyListener method: ignore (work done in keyReleased).
     * Also, KeyTyped events don't report keycodes so not good for what
     * we need to catch.
     */
    public void keyTyped(KeyEvent e) {}

    /**
     * Handle an end-of-line key (ie., they hit enter so send the text).
     */
    public void handleEndOfLine() {
	String channel = showChannels ? (String)channelComboBox.getSelectedItem() : channelDefault;
	// Get input string
	compositionArea.setCaretPosition(0);
	String text = canonicalizeLine(compositionArea.getText());
	// ignore empty input
	if (text.equals(""))
	  return;
	// Put utterance at end of history if not same as last
	if (utteranceHistory.isEmpty() ||
	    !text.equals(utteranceHistory.get(utteranceHistory.size()-1))) {
	    utteranceHistory.add(text);
	}
	// Send STOPPED-SPEAKING
	sendStoppedSpeaking(channel);
	// Send a WORD message with all the words
	sendWord(channel, text);
	// Finally an UTTERANCE message (see sendUtterance for comment)
	sendUtterance(channel, text);
	// Reset composition area
	compositionArea.setCaretPosition(0);
	compositionArea.setText("");
	// reset the pointer at end of history
	utteranceHistoryPos = utteranceHistory.size();
	// Add to transcript
	insertMsg(USR, text);
	// Ready for next
	startedSpeaking = false;
    }

    /**
     * Handle a key that navigates the utterance history.
     */
    protected void handleHistory(int keycode) {
	// count navigating through history as starting an utterance
	// (otherwise never sends started speaking.
	maybeStartedSpeaking();
	if (keycode == KeyEvent.VK_DOWN) {
	    if (!utteranceHistory.isEmpty()) {
		if (utteranceHistoryPos >= (utteranceHistory.size() - 1)) {
		    // at end of history
		    utteranceHistoryPos = utteranceHistory.size();
		    compositionArea.setText("");
		} else { // not at end
		    utteranceHistoryPos++;
		    compositionArea.setText(utteranceHistory.get(utteranceHistoryPos));
		}
	    }
	} else if (keycode == KeyEvent.VK_UP) {
	    if (!utteranceHistory.isEmpty()) {
		if (utteranceHistoryPos == 0) { // at front of history
		    // do nothing
		} else { // not at front
		    utteranceHistoryPos--;
		    compositionArea.setText(utteranceHistory.get(utteranceHistoryPos));
		}
	    }
	}
    }

    /**
     * Send the started-speaking message if we haven't already done so
     * this utterance.
     */
    protected void maybeStartedSpeaking() {
	if (!startedSpeaking) {
	    uttnum += 1;
	    String channel = showChannels ? 
		(String)channelComboBox.getSelectedItem() : channelDefault;
	    sendStartedSpeaking(channel);
	    // Don't do it again this utt
	    startedSpeaking = true;
	}
    }

    /**
     * Send message indicating that the user has started spoeaking.
     */
    protected void sendStartedSpeaking(String channel) {
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
    }
    /**
     * Send message indicating that the user has stopped speaking.
     */
    protected void sendStoppedSpeaking(String channel) {
	KQMLPerformative perf = new KQMLPerformative("tell");
	KQMLList content = new KQMLList();
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
    }
    /**
     * Send WORD message with the content of the user's utterance
     * (yes, that is usually more than one word...).
     */
    protected void sendWord(String channel, String text) {
	KQMLPerformative perf = new KQMLPerformative("tell");
	KQMLList content = new KQMLList();
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
	perf.setParameter(":content", content);
	send(perf);
    }
    /**
     * Send UTTERANCE message with the content of the user's utterance
     * (yes, this is really sort of redundant, but UTTERANCE is intended
     * to summarize a single utterance for simple transcriptors who don't
     * want to handle WORD and BACKTO).
     */
    protected void sendUtterance(String channel, String text) {
	KQMLPerformative perf = new KQMLPerformative("tell");
	KQMLList content = new KQMLList();
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
    }

    //
    // ActionListener methods
    //
    public void actionPerformed(ActionEvent evt) {
        String cmd = evt.getActionCommand();  
        if (cmd.equals("Quit")) {
	    exit(0);
	} else if (cmd.equals("48")) {
	    transcriptArea.setFont(new Font("Monospaced", Font.BOLD, 48));
	    compositionArea.setFont(new Font("Monospaced", Font.BOLD, 48));
	} else if (cmd.equals("36")) {
	    transcriptArea.setFont(new Font("Monospaced", Font.BOLD, 36));
	    compositionArea.setFont(new Font("Monospaced", Font.BOLD, 36));
	} else if (cmd.equals("24")) {
	    transcriptArea.setFont(new Font("Monospaced", Font.BOLD, 24));
	    compositionArea.setFont(new Font("Monospaced", Font.BOLD, 24));
	} else if (cmd.equals("14")) {
	    transcriptArea.setFont(new Font("Monospaced", Font.BOLD, 14));
	    compositionArea.setFont(new Font("Monospaced", Font.BOLD, 14));
	} else if (cmd.equals("12")) {
	    transcriptArea.setFont(new Font("Monospaced", Font.BOLD, 12));
	    compositionArea.setFont(new Font("Monospaced", Font.BOLD, 12));
	} else if (cmd.equals("8")) {
	    transcriptArea.setFont(new Font("Monospaced", Font.BOLD, 8));
	    compositionArea.setFont(new Font("Monospaced", Font.BOLD, 8));
	} else if (cmd.equals("6")) {
	    transcriptArea.setFont(new Font("Monospaced", Font.BOLD, 6));
	    compositionArea.setFont(new Font("Monospaced", Font.BOLD, 6));
	}
    }

    //
    // ItemListener methods
    //

    /**
     * ItemListener method: Handle checkboxes, etc.
     */
    public void itemStateChanged(ItemEvent e) {
	Object source = e.getItemSelectable();
	if (source == beepCheckBox) {
	    doBeep = beepCheckBox.isSelected();
	}
    }
    //
    // Text window methods
    //

    /**
     * Insert a message in the transcript area preceded by sender tag
     * (USR or SYS).
     */
    protected void insertMsg(String sender, String line) {
	String utt = line;
	if (dontPrintSilenceTokens) {
	    utt = line.replaceAll("<sil>","");
	    utt = canonicalizeLine(utt); // pull out extra spaces left behind
	    if (utt.startsWith(" ")) // trim space at front
		utt = utt.substring(1);
	}
	// Insert this text ahead of the user's current line
	transcriptArea.insert(sender + utt + NEWLINE, lastCaret);
	// Update new start of user's text
	// NC: ** NEWLINE is system dependent on its length!
	lastCaret += sender.length() + utt.length() + NEWLINE.length();
	// Update display
//	transcriptArea.validate();
//	transcriptScrollBar.setValue(transcriptScrollBar.getMaximum());
	SwingUtilities.invokeLater(new Runnable()
            {
                public void run()
		{
		     transcriptScrollBar.setValue(transcriptScrollBar.getMaximum());
		}
	    });
    }
    /**
     * Make sure a string is ready to be put in the transcript/sent over KQML
     * as a single utterance.
     */
    protected static String canonicalizeLine(String line)
    {
	return line.replaceAll("[\r\n\\\\]","") //remove newlines, backslashes
		   .replaceAll("\"","\\\"") //escape double quotes
	           .replaceAll("\\s+"," ") //whitespace->single space char
		   .replace("^\\s*","") //trim leading space
		   .replace("\\s*$",""); //trim trailing space
    }

    //
    // KQMLReceiver methods overriding StandardTripsModule defaults
    //

    /**
     * StandardTripsModule messaging method.
     */
    public void receiveEOF() {
	exit(0);
    }
    /**
     * StandardTripsModule messaging method.
     */
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
	    KQMLString text = (KQMLString)content.getKeywordArg(":text");
	    debug("receiveTell: text=\"" + text + "\"");
	    if (text == null) {
		errorReply(msg, "missing :text in UTTERANCE");
	    } else {
		// Display utterance unless it's from us or TextTagger (who got it from us)
		KQMLObject senderobj = msg.getParameter(":sender");
		if (senderobj != null &&
		    (!senderobj.toString().equalsIgnoreCase(name)) &&
		    (!senderobj.toString().equalsIgnoreCase("TextTagger"))) {
		    insertMsg(USR, canonicalizeLine(text.stringValue()));
		}
	    }
	} else if (content0.equalsIgnoreCase("spoken")) {
	    KQMLString text = (KQMLString)content.getKeywordArg(":what");
	    insertMsg(SYS, canonicalizeLine(text.stringValue()));
	    if (doBeep) {
		beep();
	    }
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
    /**
     * StandardTripsModule messaging method.
     */
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
	} else if (content0.equalsIgnoreCase("restart")) {
	    reset();
	    showWindow();
	} else if (content0.equalsIgnoreCase("grab")) {
	    System.err.println("tkeyboard: grab request not implemented");
	} else if (content0.equalsIgnoreCase("ungrab")) {
	    System.err.println("tkeyboard: ungrab request not implemented");
	} else if (content0.equalsIgnoreCase("add-text")) {
	    KQMLString text = (KQMLString)content.getKeywordArg(":text");
	    if (text == null) {
		errorReply(msg, "missing :text in UTTERANCE");
	    } else {
		insertMsg(USR, canonicalizeLine(text.stringValue()));
	    }
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
	    insertMsg(SYS, canonicalizeLine(str));
	} else if (content0.equalsIgnoreCase("generate")) {
	    // GENERATE messages; TODO -- remove when we have a GM
	    KQMLObject genContent = content.getKeywordArg(":content");
	    if (genContent == null) {
		errorReply(msg, "Say what?");
		return;
	    }
	    String str = genContent.toString();
	    insertMsg(SYS, canonicalizeLine(str));
	} else {
	    errorReply(msg, "bad request: " + content0);
	}
    }
    /**
     * StandardTripsModule method: Make this component's window visible.
     */
    public void showWindow(){
	frame.setState(JFrame.NORMAL);
    }
    /**
     * StandardTripsModule method: Make this component's window invisible.
     */
    public void hideWindow(){
	frame.setState(JFrame.ICONIFIED);
    }
    /**
     * StandardTripsModule method: Erase contents of windows and reset state.
     */
    public void reset(){
	transcriptArea.setText("");
	compositionArea.setText("");
	lastCaret = 0;
	// for now, keep utterance history
	utteranceHistoryPos = utteranceHistory.size();
    }

    //
    // Sound-related methods
    //
    /**
     * Clip used for beep().
     */
    protected Clip beepClip;
    /**
     * True if we've tried to initialize the beep (and so shouldn't
     * try again).
     */
    protected boolean didInitBeep = false;
    /**
     * Initialize sound for later use by beep().
     */
    protected void initBeep() {
	didInitBeep = true;
	try {
	    URL url = KeyboardManager.class.getResource("BopBeep.aiff");
	    AudioInputStream ais = AudioSystem.getAudioInputStream(url);
	    if (ais == null) {
		throw new IOException("couldn't create AudioInputStream: " + url);
	    }
	    AudioFormat	format = ais.getFormat();
	    DataLine.Info info = new DataLine.Info(Clip.class, format);
	    beepClip = (Clip)AudioSystem.getLine(info);
	    beepClip.open(ais);
	} catch (Exception ex) {
	    System.err.println("KeyboardManager.initBeep: " + ex);
	    beepClip = null;
	}
    }
    /**
     * Play a short beep sound.
     */
    protected void beep() {
	if (!didInitBeep) {
	    initBeep();
	}
	if (beepClip != null) {
	    beepClip.setFramePosition(0);
	    beepClip.start();
	}
    }

    /**
     * When run as an application, start an instance of this component.
     */
    public static void main(String argv[]) {
	new KeyboardManager(argv, true).run();
    }
}
