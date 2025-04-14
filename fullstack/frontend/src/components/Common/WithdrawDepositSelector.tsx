import { SegmentedControl } from "../ui/segmented-control"

export default function WithdrawDepositSelectorSegmented({
  setShowDeposits,
  showDeposits,
}: {
  showDeposits: boolean
  setShowDeposits: React.Dispatch<React.SetStateAction<boolean>>
}) {
  const availableOptions = ["deposits", "expenses"]

  return (
    <SegmentedControl
      defaultValue={showDeposits ? "deposits" : "expenses"}
      value={showDeposits ? "deposits" : "expenses"}
      items={availableOptions.map((option) => ({
        value: option,
        label: option.charAt(0).toUpperCase() + option.slice(1),
      }))}
      onValueChange={(value) => {
        setShowDeposits(value.value === "deposits")
      }}
    />
  )
}
