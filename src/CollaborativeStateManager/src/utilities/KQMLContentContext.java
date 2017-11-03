package utilities;

import TRIPS.KQML.KQMLList;

/**
 * A helper class for returning content and context separately
 * @author iperera
 *
 */
public class KQMLContentContext {

	private KQMLList content;
	private KQMLList context;
	
	public KQMLContentContext(KQMLList content, KQMLList context) {
		this.content = content;
		this.context = context;
	}

	public KQMLList getContent() {
		return content;
	}

	public KQMLList getContext() {
		return context;
	}
	
	

}
