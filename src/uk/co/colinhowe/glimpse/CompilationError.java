package uk.co.colinhowe.glimpse;

public abstract class CompilationError {
	private final int lineNumber;

	public int getLineNumber() {
		return lineNumber;
	}

	public CompilationError(int lineNumber) {
		this.lineNumber = lineNumber;
	}
}
