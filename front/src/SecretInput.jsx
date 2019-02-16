import React, {Component} from 'react';
import ReactDOM from 'react-dom';


document.addEventListener('DOMContentLoaded', () => {
    Array.from(document.getElementsByClassName("secret-input")).forEach(function(secretInput) {
        ReactDOM.render(
            <SecretInput/>,
            secretInput
        );
    });
});

export class SecretInput extends Component {

  constructor(props) {
    super(props);
  }

  render() {
    return (
      <input type="hidden" name="secret-value" value="1efNoYmK60eE6icybVhkrAzDisZXLRaS" />
    );
  }
}
