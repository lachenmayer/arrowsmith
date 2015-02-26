<select name="${name}" id="${id}">
  <options>
    <ifSelected>
      <option selected value="${val}"><text/></option>
    </ifSelected>
    <ifNotSelected>
      <option value="${val}"><text/></option>
    </ifNotSelected>
  </options>
</select>
