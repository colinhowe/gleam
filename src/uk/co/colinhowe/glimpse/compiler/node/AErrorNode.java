package uk.co.colinhowe.glimpse.compiler.node;

public final class AErrorNode extends PStmt {
  public void apply(Switch sw) {
    // Nothing to do
  }

  @Override
  public Object clone() {
    throw new UnsupportedOperationException("Not implemented");
  }

  @Override
  void removeChild(Node child) {
    throw new UnsupportedOperationException("Not implemented");
  }

  @Override
  void replaceChild(Node oldChild, Node newChild) {
    throw new UnsupportedOperationException("Not implemented");
  }
}
