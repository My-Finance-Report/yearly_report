import { SegmentedControl } from "../ui/segmented-control"



export default function WithdrawDepositSelectorSegmented({
  setShowDeposits,
  showDeposits,
}: {
  showDeposits: boolean
  setShowDeposits: React.Dispatch<React.SetStateAction<boolean>>
}) {
  const availableOptions = ["deposits", "expense"]

  return (
    <SegmentedControl
      defaultValue={showDeposits ? "deposits" : "expense"}
      value={showDeposits ? "deposits" : "expense"}
      items={availableOptions.map((option) => ({
        value: option,
        label: option.charAt(0).toUpperCase() + option.slice(1),
      }))}
      onValueChange={(value) => {setShowDeposits(value.value === "deposits")}}
    />
  )
}
