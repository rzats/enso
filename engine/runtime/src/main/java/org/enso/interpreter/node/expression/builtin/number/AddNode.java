package org.enso.interpreter.node.expression.builtin.number;

import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.nodes.NodeInfo;
import org.enso.interpreter.Language;
import org.enso.interpreter.node.expression.builtin.BuiltinRootNode;
import org.enso.interpreter.runtime.callable.argument.ArgumentDefinition;
import org.enso.interpreter.runtime.callable.function.Function;
import org.enso.interpreter.runtime.callable.function.FunctionSchema.CallStrategy;
import org.enso.interpreter.runtime.state.Stateful;

/** An implementation of the operator + for numbers. */
@NodeInfo(shortName = "Number.+", description = "Addition on numbers.")
public class AddNode extends BuiltinRootNode {
  private AddNode(Language language) {
    super(language);
  }

  /**
   * Executes this node.
   *
   * @param frame current execution frame
   * @return the result of adding the two operands
   */
  @Override
  public Stateful execute(VirtualFrame frame) {
    long left = (long) Function.ArgumentsHelper.getPositionalArguments(frame.getArguments())[0];
    long right = (long) Function.ArgumentsHelper.getPositionalArguments(frame.getArguments())[1];

    Object state = Function.ArgumentsHelper.getState(frame.getArguments());

    return new Stateful(state, left + right);
  }

  /**
   * Creates a two-argument function wrapping this node.
   *
   * @param language the current language instance
   * @return a function wrapping this node
   */
  public static Function makeFunction(Language language) {
    return Function.fromBuiltinRootNode(
        new AddNode(language),
        CallStrategy.DIRECT_WHEN_TAIL,
        new ArgumentDefinition(0, "this", ArgumentDefinition.ExecutionMode.EXECUTE),
        new ArgumentDefinition(1, "that", ArgumentDefinition.ExecutionMode.EXECUTE));
  }

  /**
   * Returns a language-specific name for this node.
   *
   * @return the name of this node
   */
  @Override
  public String getName() {
    return "Number.+";
  }
}
